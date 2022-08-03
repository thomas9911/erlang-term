defmodule IntegrationTest.ParseTest do
  @moduledoc """
  call the parse example and check the Rust debug output.

  Because it uses the debug format, this is not exactly stable (as in this can break between rust versions).
  """

  use ExUnit.Case, async: true
  use ExUnitProperties

  doctest IntegrationTest

  alias IntegrationTest.Parse

  test "one" do
    assert "Byte(1)" == Parse.call_binary!(1)
  end

  test "string" do
    assert "Byte(1)" == Parse.call_binary!(<<128>>)
  end

  test "map" do
    assert "Map({})" == Parse.call_binary!(%{})
  end

  test "elixir map" do
    data = %{
      [1, 2, 3] => "test",
      {"key", 3} => ["1", "2"]
    }

    # check if the ordering is stable
    assert "MapArbitrary(VecKeylist([(Tuple([String(\"key\"), Byte(3)]), List([String(\"1\"), String(\"2\")])), (Charlist([1, 2, 3]), String(\"test\"))]))" == Parse.call_binary!(data)
  end

  test "empty list" do
    assert "List([])" == Parse.call_binary!([])
  end

  test "charlist" do
    assert "Charlist([116, 101, 115, 116, 49, 50, 51])" == Parse.call_binary!('test123')
  end

  # property "works" do
  #   check all term <- binary() do
  #     assert {:ok, _} = Parse.call_binary(term)
  #   end
  # end
end
