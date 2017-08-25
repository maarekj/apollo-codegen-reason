open Jest;
open Acr_utils;
open ExpectJs;

describe "join" (fun () => {
  test "join" (fun () => {
    let list = ["joseph", "vanessa", "yona", "naéli"];
    let joined = list |> join " | ";
    expect joined |> toEqual "joseph | vanessa | yona | naéli"
  });
});

describe "capitalize" (fun () => {
  test "with lowercased word" (fun () => expect (capitalize "joseph maarek") |> toEqual "Joseph maarek");
  test "with already capitalized word" (fun () => expect (capitalize "Joseph maarek") |> toEqual "Joseph maarek");
  test "with uppercased word" (fun () => expect (capitalize "VANESSA") |> toEqual "VANESSA")
});

describe "clear_space" (fun () => {
  test "with empty string" (fun () => expect (clear_space "") |> toEqual "");
  test "with normal string" (fun () => expect (clear_space "j'aime ma femme") |> toEqual "j'aime ma femme");
  test "with many spaces" (fun () => expect (clear_space "j'aime   ma    femme") |> toEqual "j'aime ma femme");
  test
    "with many spaces and many line breaks"
    (fun () => expect (clear_space "j'aime\n\t   \nma\n\nfemme") |> toEqual "j'aime ma femme")
});

describe "trim_space" ( fun () => {
  test "left_trim_space with empty string" (fun () => expect (left_trim_space "") |> toEqual "");
  test "right_trim_space with empty string" (fun () => expect (right_trim_space "") |> toEqual "");
  test "trime_spacewith empty string" (fun () => expect (trim_space "") |> toEqual "");
  test "left_trim_space" (fun () => expect (left_trim_space "   joseph maarek   ") |> toEqual "joseph maarek   ");
  test
    "right_trim_space" (fun () => expect (right_trim_space "   joseph maarek   ") |> toEqual "   joseph maarek");
  test "trim_space" (fun () => expect (trim_space "   joseph maarek   ") |> toEqual "joseph maarek")
});

describe "first_or_fail" (fun () => {
  test "with list not empty" (fun () => expect (first_or_fail [1, 2, 3]) |> toEqual 1);
  test "with list not empty" (fun () => expect (fun () => first_or_fail []) |> toThrowMessage "The list must be not empty");
});