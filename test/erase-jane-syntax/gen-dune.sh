function check-file {
    local f="$1"

    ref="$f.ref"
    out="$f.out"

    cat <<EOF
(rule
  (target $out)
  (action
    (with-stdout-to %{target}
      (run %{project_root}/bin/main.exe style --erase-jane-syntax %{dep:$f}))))

(rule
  (alias diff-$f)
  (action (diff %{dep:$ref} %{dep:$out})))

(alias
  (name runtest)
  (deps (alias diff-$f)))

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
