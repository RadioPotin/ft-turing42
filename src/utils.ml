let error msg =
  Format.eprintf "error: %s@." msg;
  exit 1

let err_invalid_type std t =
  Format.eprintf "error: Invalid type %s@."
    (Yojson.Basic.pretty_to_string ~std t);
  exit 1
