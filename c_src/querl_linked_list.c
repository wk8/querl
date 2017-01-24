#include "erl_nif.h"

#include <limits.h>

static ErlNifResourceType* querl_linked_list_resource;

typedef struct querl_linked_list_t {
  // that's the env containing the value
  // NULL for the root node
  ErlNifEnv* env;
  ERL_NIF_TERM value;
  struct querl_linked_list_t* next;
  struct querl_linked_list_t* previous;
  // non-0 only for root nodes, and then it provides a (non-fool proof, but
  // sufficient) mechanism of ensuring that the erlang code does not assume the
  // immutability of querl_linked_list references
  unsigned int root_version;
} querl_linked_list_t;

// in all the funs below, we take a root as 1st argument, and its version as
// second; this macro unpacks both, and checks that we have indeed a root node
// at the expected version
#define UNPACK_ROOT_AND_CHECK_VERSION                                              \
  querl_linked_list_t* root;                                                       \
  unsigned int expected_version;                                                   \
  if (!enif_get_resource(env, argv[0], querl_linked_list_resource, (void **)&root) \
      || !enif_get_uint(env, argv[1], &expected_version)                           \
      || !expected_version                                                         \
      || root->root_version != expected_version) {                                 \
    return enif_make_badarg(env);                                                  \
  }

#define BUMP_AND_PACK_ROOT_VERSION                                                 \
  if (root->root_version == UINT_MAX) {                                            \
    root->root_version = 1;                                                        \
  } else {                                                                         \
    root->root_version++;                                                          \
  }                                                                                \
  ERL_NIF_TERM root_version_term = enif_make_uint(env, root->root_version);

static ERL_NIF_TERM querl_linked_list_new(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
  querl_linked_list_t* root = enif_alloc_resource(querl_linked_list_resource, sizeof(querl_linked_list_t));
  root->env = NULL;
  root->value = 0;
  root->previous = root;
  root->next = root;
  root->root_version = 1;

  ERL_NIF_TERM resource = enif_make_resource(env, root);
  enif_release_resource(root);

  return enif_make_tuple2(env, resource, enif_make_uint(env, root->root_version));
}

static ERL_NIF_TERM querl_linked_list_append(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
  UNPACK_ROOT_AND_CHECK_VERSION

  querl_linked_list_t* previous_last_node = root->previous;

  querl_linked_list_t* new_node = enif_alloc_resource(querl_linked_list_resource, sizeof(querl_linked_list_t));
  new_node->env = enif_alloc_env();
  new_node->value = enif_make_copy(new_node->env, argv[2]);
  new_node->next = root;
  new_node->previous = previous_last_node;
  new_node->root_version = 0;

  root->previous = new_node;
  previous_last_node->next = new_node;

  ERL_NIF_TERM resource = enif_make_resource(env, new_node);
  enif_release_resource(new_node);

  BUMP_AND_PACK_ROOT_VERSION
  return enif_make_tuple2(env, resource, root_version_term);
}

static ERL_NIF_TERM querl_linked_list_to_list(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
  UNPACK_ROOT_AND_CHECK_VERSION

  querl_linked_list_t* current = root->previous;
  ERL_NIF_TERM list = enif_make_list(env, 0), copied_value;

  while(!current->root_version) {
    copied_value = enif_make_copy(env, current->value);
    list = enif_make_list_cell(env, copied_value, list);
    current = current->previous;
  }

  return list;
}

static ERL_NIF_TERM querl_linked_list_to_reversed_list(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
  UNPACK_ROOT_AND_CHECK_VERSION

  querl_linked_list_t* current = root->next;
  ERL_NIF_TERM list = enif_make_list(env, 0), copied_value;

  while(!current->root_version) {
    copied_value = enif_make_copy(env, current->value);
    list = enif_make_list_cell(env, copied_value, list);
    current = current->next;
  }

  return list;
}

// common helper function for both querl_linked_list_reversed_out and
// querl_linked_list_reversed_empty below
static ERL_NIF_TERM reversed_out(ErlNifEnv* env, const ERL_NIF_TERM argv[], const ErlNifUInt64 requested_size)
{
  UNPACK_ROOT_AND_CHECK_VERSION

  querl_linked_list_t *current = root->next;
  ERL_NIF_TERM list = enif_make_list(env, 0), copied_value;

  ErlNifUInt64 current_size = 0;
  while(!current->root_version && current_size < requested_size) {
    copied_value = enif_make_copy(env, current->value);
    list = enif_make_list_cell(env, copied_value, list);
    current = current-> next;
    current_size++;
  }
  root->next = current;
  current->previous = root;

  BUMP_AND_PACK_ROOT_VERSION
  return enif_make_tuple3(env, list, enif_make_uint64(env, current_size), root_version_term);
}

static ERL_NIF_TERM querl_linked_list_reversed_out(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
  ErlNifUInt64 requested_size;

  if (!enif_get_uint64(env, argv[2], &requested_size)) {
    return enif_make_badarg(env);
  }

  return reversed_out(env, argv, requested_size);
}

static ERL_NIF_TERM querl_linked_list_reversed_empty(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
  return reversed_out(env, argv, ULLONG_MAX);
}

static ERL_NIF_TERM querl_linked_list_remove(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
  UNPACK_ROOT_AND_CHECK_VERSION

  querl_linked_list_t* node, *previous, *next;
 
  if (!enif_get_resource(env, argv[2], querl_linked_list_resource, (void **)&node)
      || node->root_version) {
    return enif_make_badarg(env);
  }

  previous = node->previous;
  next = node->next;

  previous->next = next;
  next->previous = previous;

  BUMP_AND_PACK_ROOT_VERSION
  return root_version_term;
}

void destroy_querl_linked_list_resource(ErlNifEnv* env, void* obj)
{
  querl_linked_list_t* node = (querl_linked_list_t*) obj;

  if (node->env) {
    enif_free_env(node->env);
  }
}

static int querl_linked_list_load(ErlNifEnv *env, void **priv_data, ERL_NIF_TERM load_info)
{
  querl_linked_list_resource = enif_open_resource_type(env, NULL, "querl_linked_list",
                                                       destroy_querl_linked_list_resource,
                                                       ERL_NIF_RT_CREATE | ERL_NIF_RT_TAKEOVER,
                                                       NULL);
  return querl_linked_list_resource ? 0 : 1;
}

static int querl_linked_list_upgrade(ErlNifEnv* env, void** priv_data, void** old_priv_data, ERL_NIF_TERM load_info)
{
  // nothing to do
  return 0;
}

static ErlNifFunc querl_linked_list_nif_funcs[] = {
    {"new",              0, querl_linked_list_new},
    {"append",           3, querl_linked_list_append},
    {"to_list",          2, querl_linked_list_to_list},
    {"to_reversed_list", 2, querl_linked_list_to_reversed_list},
    {"reversed_out",     3, querl_linked_list_reversed_out},
    {"reversed_empty",   2, querl_linked_list_reversed_empty},
    {"remove",           3, querl_linked_list_remove}
};

ERL_NIF_INIT(querl_linked_list, querl_linked_list_nif_funcs,
             querl_linked_list_load, NULL, querl_linked_list_upgrade, NULL)
