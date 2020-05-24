defmodule TermGenerator do
    
    @base_path "bins"
    def generate() do
        save(2, "small_int")
        save(1234578, "int")
        save(nil, "nil")
        save(false, "false")
        save(true, "true")
        save([], "empty_list")
        save("just some text", "small_string")
        # save_large_string("large_string")
        save(12.515, "float")
        save(:oddţ, "odd_atom")
        save(TermGenerator, "module_name")
        save([1,2,3,4], "number_list")
        save([1, "some", 2, "text"], "mixed_list")
        save([1, 6 | 2], "improper_list")
        save(%{just: "some key", other: "value"}, "atom_map")
        save([just: "some key", other: "value"], "keyword")
        save({"test", "testing"}, "tuple")
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
end


TermGenerator.generate()