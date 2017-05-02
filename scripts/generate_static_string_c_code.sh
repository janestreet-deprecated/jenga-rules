#!/bin/bash

set -e -o pipefail

function_name=$1

cat <<EOF
#include <caml/mlvalues.h>
#include <caml/memory.h>
#include <caml/alloc.h>

CAMLprim value ${function_name} (value unit __attribute__ ((unused)))
{
  char v[] = { $({ cat; printf "\0"; } | /usr/bin/xxd -i | tr -d '\n') };

  return(caml_copy_string(v));
}
EOF
