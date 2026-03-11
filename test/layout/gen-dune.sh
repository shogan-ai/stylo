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
      (run %{project_root}/bin/main.exe style --width 90 %{dep:$f}))))

(rule
  (target $name.snd-run)
  (action
    (with-stdin-from %{dep:$out}
      (with-stdout-to %{target}
        (run %{project_root}/bin/main.exe style --width 90 --stdin $name)))))

(rule
  (alias runtest)
  (action (diff %{dep:$ref} %{dep:$out})))

; idempotence check
(rule
  (alias runtest)
  (action (diff %{dep:$out} %{dep:$name.snd-run})))

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
