open Core.Std
open Import

let ( *>>| ) = Dep.map
let relative = Path.relative

let format_path_list xs =
  String.concat (List.map xs ~f:(fun path ->
    sprintf "%s\n" (Path.to_string path)
  ))

module F(X : sig val dir : Path.t end) = struct
  open X

    let rules =
      let buildable_targets_list = relative ~dir "buildable_targets.list" in
      [
        Rule.create ~targets:[buildable_targets_list] (
          Dep.buildable_targets ~dir *>>| fun paths ->
          Action.save (format_path_list paths) ~target:buildable_targets_list
        );
        Rule.alias (Alias.create ~dir "info") [
          Dep.path buildable_targets_list;
        ];
      ]

end

let write ~dir =
  let module M = F (struct let dir = dir end) in
  M.rules
