function check-file {
    local f="$1"
    local name=$(basename $(basename $f ".mli") ".ml")
    cat <<EOF
(rule
  (target $name.out)
  (action
    (with-stdout-to %{target}
      (run ../../bin/stylo.exe %{dep:$f}))))

(rule
  (target $name.snd-run)
  (action
    (with-stdout-to %{target}
      (run ../../bin/stylo.exe %{dep:$name.out}))))

(rule
  (alias runtest)
  (action (diff %{dep:$name.ref} %{dep:$name.out})))

; idempotence check
(rule
  (alias runtest)
  (action (diff %{dep:$name.out} %{dep:$name.snd-run})))

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
