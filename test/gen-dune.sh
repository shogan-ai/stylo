function implementation_file {
    local f="$1"
    name=$(basename -s ".ml" $f)
    cat <<EOF
(rule
  (with-stdout-to printed-$f
    (run ../bin/stylo.exe %{dep:$f})))

(rule
  (alias $name)
  (action (diff %{dep:$f} %{dep:printed-$f})))

(alias
  (name all-tests)
  (deps (alias $name)))

;; ---

EOF
}

function fuzzer_output {
    local BASE=$(basename -s '.mls' $1)
    local FILENAMES=()

    for j in $(seq 1 $(wc -l $1 | cut -d' ' -f1)); do
        (( i = j - 1 ))
        f="$BASE-$i"
        FILENAMES[$i]="$f"
    done

    cat <<EOF
(rule
  (targets ${FILENAMES[@]})
  (action (run ../tools/spread_fuzzer_tests.exe %{dep:$1})))

(rule
  (alias $BASE)
  (deps ${FILENAMES[@]})
  (action (run ../bin/stylo.exe -only-check %{deps})))
;; ---

EOF
}

for f in $@; do
    case $f in
        *.ml)
            implementation_file "$f"
            ;;

        *.mls)
            fuzzer_output "$f"
            ;;

        *)
            ;;
    esac
done
