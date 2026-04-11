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
