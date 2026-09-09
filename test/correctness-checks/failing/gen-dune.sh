function check-file {
    local f="$1"
    cat <<EOF
(rule
  (alias runtest)
  (action
    (with-accepted-exit-codes 123
      (ignore-stdout
      (ignore-stderr
        (run ../../../bin/main.exe style --idempotence-check --ast-check
            %{dep:$f}))))))

EOF
}

for f in $@; do
    # dune >= 3.24 expands %{deps} with a leading "./"
    f="${f#./}"
    case $f in
        *.ml|*.mli)
            check-file "$f"
            ;;

        *)
            ;;
    esac
done
