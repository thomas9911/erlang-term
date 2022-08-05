defmodule IntegrationTest.Parse do
  @binary_path "../target/release/examples/parse"

  def call_binary!(term) do
    {:ok, out} = call_binary(term)
    out
  end

  def call_binary(term) do
    bin_path()
    |> System.cmd([
      inspect(:erlang.term_to_binary(term), limit: :infinity, print_limit: :infinity)
    ])
    |> as_result()
  end

  defp as_result({"Ok(" <> rest, 0}) do
    {:ok, rest |> String.trim() |> String.slice(0..-2//1)}
  end

  defp as_result(other) do
    {:error, other}
  end

  def bin_path do
    :os.type()
    |> case do
      {:win32, _} -> @binary_path <> ".exe"
      _ -> @binary_path
    end
    |> Path.expand()
  end
end
