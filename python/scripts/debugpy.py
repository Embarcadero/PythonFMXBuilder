import debugpy

subprocEnv = {
    "LD_LIBRARY_PATH": "%lib_path%",
    "PYTHONHOME": "%python_home%",
    "PATH": "%path%",
    "TMPDIR": "%tmp_dir%",
}

def main():
    debugpy.configure(pythonEnv=subprocEnv)
    debugpy.listen(("%host%", %port%))
    debugpy.wait_for_client()

if __name__ == "__main__":
    main()