load /usr/lib/bats-support/load
load /usr/lib/bats-assert/load

@test "Caching rules" {
  man=$HOME/.local/share/man
  export MANPATH=$man

  foo=$man/man1/foo.1
  foo_html=man1/foo.1.html
  mkdir -p "$(dirname "$foo")"
  rm -f "$foo"

  dist=$(mktemp -d)
  bluebook -C "$dist" --no-color

  # Ensure fully-cached
  run bluebook -C "$dist" --no-color
  assert_line --index 0 --regexp "^Build completed in .*s$"

  # New man-page => build just the section and total index and itself
  touch "$foo"
  run sh -c "bluebook -C '$dist' --no-color | grep '^# ' | sort"
  assert_output <<EOM
# $foo
# index.html
# index.json
# $foo_html
# man1/index.html
# man1/index.json
EOM

  # Changed content => build only it
  echo "Some content" >"$foo"
  run sh -c "bluebook -C '$dist' --no-color | grep '^# ' | sort"
  assert_output <<EOM
# $foo
# $foo_html
EOM

  # Removed man-page => build only the necessary indexes
  rm "$foo"
  run sh -c "bluebook -C '$dist' --no-color | grep '^# ' | sort"
  assert_output <<EOM
# index.html
# index.json
# man1/index.html
# man1/index.json
EOM
}
