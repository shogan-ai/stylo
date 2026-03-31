function check-file {
    local f="$1"
    local name=$(basename "$f")

    ref="$name.stylo"
    out="$name.out"

    cat <<EOF
(rule
  (target $out)
  (action
    (with-stdout-to %{target}
      (run %{project_root}/bin/main.exe style --idempotence-check --width 90 %{dep:$f}))))

(rule
  (alias runtest)
  (action (diff %{dep:$ref} %{dep:$out})))

EOF
}

for f in $@; do
    case $f in
        *.ml|*.mli)
            check-file "$f"
            ;;

        *)
            ;;
    esac
done
