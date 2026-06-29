let deps ~switch ~dir =
  Opam_run.run_opam
    [
      "install";
      "--switch";
      switch;
      "--deps-only";
      "--with-test";
      dir;
      "--yes";
      "--no-depexts";
    ]
