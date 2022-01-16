defmodule RustlerTest do
  use Rustler, otp_app: :rustler_test, crate: "rustlertest"

  # When your NIF is loaded, it will override this function.
  def add(_a, _b), do: :erlang.nif_error(:nif_not_loaded)
  def raw_decode(_a), do: :erlang.nif_error(:nif_not_loaded)
  def decode(_a), do: :erlang.nif_error(:nif_not_loaded)
end
