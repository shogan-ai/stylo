function check-parentheses {
    local f="$1"
    local width="$2"
    local name="$3"
    local mode="$4"
    shift 4
    local mref="$name.stylo-$mode"
    local mout="$name.$mode.out"

    cat <<EOF
(rule
  (target $mout)
  (action
    (with-stdout-to %{target}
      (run %{project_root}/bin/main.exe style --idempotence-check $@ --width $width %{dep:$f}))))

(rule
  (alias runtest)
  (action (diff %{dep:$mref} %{dep:$mout})))

EOF

}

function check-file {
    local f="$1"
    local name=$(basename "$f")

    local width="90"
    if [[ $(echo $name | cut -d. -f3) ]]; then
        width=$(echo $name | cut -d. -f2)
    fi

    ref="$name.stylo"
    out="$name.out"

    cat <<EOF
(rule
  (target $out)
  (action
    (with-stdout-to %{target}
      (run %{project_root}/bin/main.exe style --idempotence-check --width $width %{dep:$f}))))

(rule
  (alias runtest)
  (action (diff %{dep:$ref} %{dep:$out})))

EOF

    check-parentheses "$f" "$width" "$name" remove --remove-parentheses
    check-parentheses "$f" "$width" "$name" normalize --remove-parentheses --insert-parentheses
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
