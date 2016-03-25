defmodule Aggcheck.Mixfile do
  use Mix.Project

  def project do
    [app: :aggcheck,
     version: "0.0.2",
     language: :erlang,
     escript: escript,
     deps: deps]
  end

  def application do
    [applications: [], mod: {:aggcheck, []}]
  end

  defp escript do
    [ main_module: :aggcheck_cli,
      embedd_elixir: false ]
  end

  defp deps do
    [
      {:mix_erlang_tasks, "0.1.0"},
      {:jsx, "2.8.0"},
      {:getopt, "0.8.2"},
    ]
  end
end
