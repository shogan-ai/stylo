function check-file {
    local f="$1"
    local name=$(basename $(basename $f ".mli") ".ml")
    cat <<EOF
(rule
  (target $name.out)
  (action
    (with-stdout-to $name.out
      (run ../../bin/stylo.exe %{dep:$f}))))

(rule
  (alias runtest)
  (action (diff %{dep:$name.ref} %{dep:$name.out})))

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
