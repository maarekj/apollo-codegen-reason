open Jest;

open Acr_utils;

open ExpectJs;

describe(
  "join",
  () =>
    test(
      "join",
      () => {
        let list = ["joseph", "vanessa", "yona", "naéli"];
        let joined = list |> join(" | ");
        expect(joined) |> toEqual("joseph | vanessa | yona | naéli")
      }
    )
);

describe(
  "capitalize",
  () => {
    test(
      "with lowercased word",
      () => expect(capitalize("joseph maarek")) |> toEqual("Joseph maarek")
    );
    test(
      "with already capitalized word",
      () => expect(capitalize("Joseph maarek")) |> toEqual("Joseph maarek")
    );
    test("with uppercased word", () => expect(capitalize("VANESSA")) |> toEqual("VANESSA"))
  }
);

describe(
  "clear_space",
  () => {
    test("with empty string", () => expect(clear_space("")) |> toEqual(""));
    test(
      "with normal string",
      () => expect(clear_space("j'aime ma femme")) |> toEqual("j'aime ma femme")
    );
    test(
      "with many spaces",
      () => expect(clear_space("j'aime   ma    femme")) |> toEqual("j'aime ma femme")
    );
    test(
      "with many spaces and many line breaks",
      () => expect(clear_space("j'aime\n\t   \nma\n\nfemme")) |> toEqual("j'aime ma femme")
    )
  }
);

describe(
  "trim_space",
  () => {
    test("left_trim_space with empty string", () => expect(left_trim_space("")) |> toEqual(""));
    test("right_trim_space with empty string", () => expect(right_trim_space("")) |> toEqual(""));
    test("trime_spacewith empty string", () => expect(trim_space("")) |> toEqual(""));
    test(
      "left_trim_space",
      () => expect(left_trim_space("   joseph maarek   ")) |> toEqual("joseph maarek   ")
    );
    test(
      "right_trim_space",
      () => expect(right_trim_space("   joseph maarek   ")) |> toEqual("   joseph maarek")
    );
    test("trim_space", () => expect(trim_space("   joseph maarek   ")) |> toEqual("joseph maarek"))
  }
);

describe(
  "first_or_fail",
  () => {
    test("with list not empty", () => expect(first_or_fail([1, 2, 3])) |> toEqual(1));
    test(
      "with list empty",
      () => expect(() => first_or_fail([])) |> toThrowMessage("The list must be not empty")
    )
  }
);