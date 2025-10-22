function check-file {
    local f="$1"
    cat <<EOF
(rule
  (alias runtest)
  (action
    (run ../../bin/stylo.exe -ast-check %{dep:$f})))

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
