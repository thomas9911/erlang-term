defmodule RustlerTestTest do
  use ExUnit.Case
  doctest RustlerTest

  describe "raw term raw_decode" do
    test "bit" do
      assert "Ok(SmallInt(123))" == RustlerTest.raw_decode(123)
    end

    test "bit 2" do
      assert "Ok(SmallInt(2))" == RustlerTest.raw_decode(2)
    end

    test "float" do
      assert "Ok(Float(0.362))" == RustlerTest.raw_decode(0.362)
    end

    test "integer" do
      assert "Ok(Int(123456))" == RustlerTest.raw_decode(123_456)
    end

    test "integer 2" do
      assert "Ok(Int(-2))" == RustlerTest.raw_decode(-2)
    end

    test "large integer" do
      assert "Ok(LargeBigInt(123456789101112131415161718192021222324))" ==
               RustlerTest.raw_decode(123_456_789_101_112_131_415_161_718_192_021_222_324)
    end

    test "bytes" do
      assert "Ok(Binary([1, 2, 3, 4, 5]))" == RustlerTest.raw_decode(<<1, 2, 3, 4, 5>>)
    end

    test "string" do
      assert "Ok(Binary([115, 111, 109, 101, 32, 116, 101, 120, 116]))" ==
               RustlerTest.raw_decode("some text")
    end

    test "atom map" do
      assert ~s|Ok(Map([(Atom("test"), SmallInt(1)), (Atom("true"), Atom("false"))]))| ==
               RustlerTest.raw_decode(%{test: 1, true: false})
    end

    test "string map" do
      assert ~s|Ok(Map([(Binary([116, 101, 115, 116]), SmallInt(1)), (Binary([116, 114, 117, 101]), Atom("false"))]))| ==
               RustlerTest.raw_decode(%{"test" => 1, "true" => false})
    end

    test "empty list" do
      assert "Ok(Nil)" == RustlerTest.raw_decode([])
    end

    test "list" do
      assert "Ok(List([Atom(\"true\"), Float(0.6), Binary([116, 101, 115, 116])]))" ==
               RustlerTest.raw_decode([true, 0.6, "test"])
    end

    test "tuple" do
      assert "Ok(LargeTuple([Atom(\"ok\"), Atom(\"true\"), Float(0.6), Binary([116, 101, 115, 116])]))" ==
               RustlerTest.raw_decode({:ok, true, 0.6, "test"})
    end

    test "function" do
      assert "Err({error, invalid_term})" == RustlerTest.raw_decode(fn -> :ok end)
    end

    test "process" do
      assert "Err({error, invalid_term})" == RustlerTest.raw_decode(self())
    end

    test "nested error" do
      assert "Err({error, invalid_term})" == RustlerTest.raw_decode([true, 0.6, self()])
    end
  end

  describe "term decode" do
    test "bit" do
      assert "Ok(Byte(123))" == RustlerTest.decode(123)
    end

    test "bit 2" do
      assert "Ok(Byte(2))" == RustlerTest.decode(2)
    end

    test "float" do
      assert "Ok(Float(0.362))" == RustlerTest.decode(0.362)
    end

    test "integer" do
      assert "Ok(Int(123456))" == RustlerTest.decode(123_456)
    end

    test "integer 2" do
      assert "Ok(Int(-2))" == RustlerTest.decode(-2)
    end

    test "large integer" do
      assert "Ok(BigInt(123456789101112131415161718192021222324))" ==
               RustlerTest.decode(123_456_789_101_112_131_415_161_718_192_021_222_324)
    end

    test "bytes" do
      assert "Ok(Bytes([1, 2, 3, 4, 5]))" == RustlerTest.decode(<<1, 2, 3, 4, 5>>)
    end

    test "string" do
      assert ~s|Ok(String("some text"))| == RustlerTest.decode("some text")
    end

    test "atom map" do
      map = RustlerTest.decode(%{test: 1, true: false})

      assert ~s|Ok(Map({"test": Byte(1), "true": Bool(false)}))| == map or
               ~s|Ok(Map({"true": Bool(false), "test": Byte(1)}))| == map
    end

    test "string map" do
      map = RustlerTest.decode(%{"test" => 1, "true" => false})

      assert ~s|Ok(Map({"test": Byte(1), "true": Bool(false)}))| == map or
               ~s|Ok(Map({"true": Bool(false), "test": Byte(1)}))| == map
    end

    test "empty list" do
      assert "Ok(List([]))" == RustlerTest.decode([])
    end

    test "list" do
      assert ~s|Ok(List([Bool(true), Float(0.6), String("test")]))| ==
               RustlerTest.decode([true, 0.6, "test"])
    end

    test "tuple" do
      assert "Ok(Tuple([Atom(\"ok\"), Bool(true), Float(0.6), String(\"test\")]))" ==
               RustlerTest.decode({:ok, true, 0.6, "test"})
    end

    test "function" do
      assert "Err({error, invalid_term})" == RustlerTest.decode(fn -> :ok end)
    end

    test "process" do
      assert "Err({error, invalid_term})" == RustlerTest.decode(self())
    end

    test "nested error" do
      assert "Err({error, invalid_term})" == RustlerTest.decode([true, 0.6, self()])
    end
  end
end
