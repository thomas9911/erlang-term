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

  test "bytes" do
    assert "Bytes([128])" == Parse.call_binary!(<<128>>)
  end

  test "string" do
    assert "String(\"test\")" == Parse.call_binary!("test")
  end

  test "map" do
    assert "Map({})" == Parse.call_binary!(%{})
  end

  test "elixir map" do
    data = %{
      [1, 2, 3] => "test",
      {"key", 3} => ["1", "2"]
    }

    option_one =
      "Map({Charlist([1, 2, 3]): String(\"test\"), Tuple([String(\"key\"), Byte(3)]): List([String(\"1\"), String(\"2\")])})"

    option_two =
      "Map({Tuple([String(\"key\"), Byte(3)]): List([String(\"1\"), String(\"2\")]), Charlist([1, 2, 3]): String(\"test\")})"

    assert Parse.call_binary!(data) in [option_one, option_two]
  end

  test "empty list" do
    assert "List([])" == Parse.call_binary!([])
  end

  test "charlist" do
    assert "Charlist([116, 101, 115, 116, 49, 50, 51])" == Parse.call_binary!('test123')
  end

  property "term property" do
    check all term <- term() do
      assert {:ok, _} = Parse.call_binary(term)
    end
  end

  property "bitstring property" do
    check all term <- bitstring() do
      assert {:ok, _} = Parse.call_binary(term)
    end
  end

  property "iolist property" do
    check all term <- iolist() do
      assert {:ok, _} = Parse.call_binary(term)
    end
  end
end
