function check-file {
    local f="$1"
    local name=$(basename "$f")

    local width="90"
    if [[ $(echo $name | cut -d. -f3) ]]; then
        width=$(echo $name | cut -d. -f2)
    fi

    cat <<EOF
(rule
  (alias runtest)
  (action
    (ignore-stdout
      (run ../../../bin/main.exe style --idempotence-check --ast-check
          --width $width %{dep:$f}))))

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
