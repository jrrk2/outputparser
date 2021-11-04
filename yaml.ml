#require "yaml" ;;
#require "yaml.unix" ;;

Yaml.of_string "foo";;
Yaml.of_string "- foo";;
Yaml.to_string (`O ["foo1", `String "bar1"; "foo2", `Float 1.0]);;
Yaml_unix.to_file Fpath.(v "my.yml") (`String "bar") ;;
Yaml_unix.of_file Fpath.(v "my.yml");;
Yaml_unix.of_file_exn Fpath.(v "my.yml");;
