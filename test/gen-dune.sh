for f in $@; do
    case $f in
        *.ml)
            name=$(basename -s ".ml" $f)
            cat <<EOF
(rule
  (with-stdout-to nocomment-$f
    (run ../tools/strip_comments.exe $f)))

(rule
  (with-stdout-to normalized-$f
    (bash "cat %{dep:nocomment-$f} | tr -s '[:space:]' '\n'")))

(rule
  (with-stdout-to printed-$f
    (run ../bin/stylo.exe nocomment-$f)))

(rule
  (alias $name)
  (action (diff normalized-$f printed-$f)))

(alias
  (name all-tests)
  (deps (alias $name)))
EOF
            ;;

        *)
            ;;
    esac
done
