defmodule TermGenerator do
    
    @base_path "bins"
    def generate() do
        save(2, "small_int")
        save(-2, "small_negative_int")
        save(1234578, "int")
        save(-1234578, "negative_int")
        save(nil, "nil")
        save(false, "false")
        save(true, "true")
        save([], "empty_list")
        save("just some text", "small_string")
        save(<<1,2,3,4>>, "binary")
        # save_large_string("large_string")
        save(12.515, "float")
        save(:oddţ, "odd_atom")
        save(TermGenerator, "module_name")
        save([1,2,3,4], "number_list")
        save([1, "some", 2, "text"], "mixed_list")
        save([1, 6 | 2], "improper_list")
        save(%{just: "some key", other: "value"}, "atom_map")
        save(%{
            "float" => 3.14,
            ["list as a key"] => ["another", %{test: false}],
            1 => "one",
            :tuple => {1, :more},
            "large" => 123456789123456789,
            "nested" => %{  "ok" => [] }
        }, "map")
        save([just: "some key", other: "value", just: 1234], "keyword")
        save({"test", "testing"}, "tuple")
        save(123456789123456789123456789, "small_big_int")
        save(Exp.exp(999,999), "large_big_int")
        # save(&testing/2, "function")
        save(self(), "pid")

        path = System.find_executable("echo")
        port = Port.open({:spawn_executable, path}, [:binary, args: ["hello world"]])
        # save(port, "port")
        Port.close(port)

        # save(make_ref(), "ref")

    end

    def save(object, name) do
        save_to_disk(object, "#{@base_path}/#{name}.bin")
    end

    def save_to_disk(object, path) do
        File.write!(path, :erlang.term_to_binary(object))
    end 

    def save_large_string(path) do
        # generate some text
        large_text = """
        """

        save(large_text, path)
    end

    defp testing(_, _), do: nil
end


defmodule Exp do
  def exp(x,y) when is_integer(x) and is_integer(y) and y>=0 do
    exp_int(x,y)
  end
 
  defp exp_int(_,0), do: 1
  defp exp_int(x,y), do: Enum.reduce(1..y, 1, fn _,acc -> x * acc end)
end

TermGenerator.generate()