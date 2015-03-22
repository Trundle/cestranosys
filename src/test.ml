open OUnit2

let strip_tag test_ctx =
  assert_equal "spam" (Cestranosys.strip_tag "spam:42")

let strip_tag_with_registry test_ctx =
  assert_equal "hub.example.org/spam" (Cestranosys.strip_tag "hub.example.org/spam:42")

let strip_tag_without_tag test_ctx =
  assert_equal "spam" (Cestranosys.strip_tag "spam")

let suite =
  "suite" >:::
    ["strip_tag" >:: strip_tag;
     "strip_tag with registry in name" >:: strip_tag_with_registry;
     "strip_tag without tag in name" >:: strip_tag_without_tag]

let () =
  run_test_tt_main suite
