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
    BASE=$(basename -s '.mls' $1)

    for j in $(seq 1 $(wc -l $1 | cut -d' ' -f1)); do
        (( i = j - 1 ))
        f="$BASE-$i"
        FILENAMES[$i]="$f"
    done

    cat <<EOF
(rule
  (write-file $BASE.sh
        "#!/bin/bash

(( j = 0 ))
while read line; do
    echo \$line > $BASE-\$j
    (( j++ ))
done"))

(rule
    (targets ${FILENAMES[@]})
    (action
      (progn
        (bash "chmod +x %{dep:./$BASE.sh}")
        (with-stdin-from %{dep:$1}
          (run %{dep:./$BASE.sh})))))

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
