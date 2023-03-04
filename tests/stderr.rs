//! Tests of output to actual stderr
//!
//! Checks that our test cases
//! produce the expected direct output to stderr.
//!
//! Direct stderr output is from things like
//! the `dbg` template option.
//!
//! We reuse some of the `trybuild`'s ui test cases, from `tests/ui/`;
//! see `tests/stderr/stderr-lib.rs` for the list.
//!
//! We must do this separately because `trybuild`'s `*.stderr` files
//! do not actually contain real stderr output.
//! The contents of those files is reconstructed from
//! rustc's JSON message output, which includes only actual compiler errors,
//! including from `compile_error!` and `.into_compile_error()`.
//! It does *not* include anything printed by `eprintln` in a proc macro.
//! `trybuild` silently discards that.
//!
//! So we reimplement, effectively, some of what `trybuild` does, here.
//!
//! *All* the output from all the test cases listed in `stderr-lib.rs`
//! is concatenated into the output, in order.
//! That avoids us having multiple test crates like this one.
///
/// `combined.real-stderr` does *not* contain compiler messages.
/// We suppress those, here, and rely on the `ui` tests to check them.
use std::env;
use std::ffi::OsString;
use std::fs::{self, File};
use std::process::{self, Command, Stdio};

/// Get `name` from the environment, sensibly
fn env(name: &str) -> Option<String> {
    env::var_os(name)
        .map(OsString::into_string)
        .transpose()
        .unwrap()
}

#[test]
fn stderr() {
    let outer_cwd = env::current_dir().unwrap();
    let outer_cwd = outer_cwd.to_str().unwrap();
    eprintln!("outer cwd {}", outer_cwd);

    let outer_manifest_dir = env::var("CARGO_MANIFEST_DIR").unwrap();
    eprintln!("outer manifest dir {}", outer_manifest_dir);
    let src_root = outer_manifest_dir.rsplit_once('/').unwrap().0;
    eprintln!("src root {}", src_root);

    let build_cwd = outer_cwd.rsplit_once('/').unwrap().0;
    eprintln!("build cwd {}", build_cwd);

    let inner_cwd = format!("{}/target/tests/derive-adhoc-stderr", build_cwd);
    eprintln!("inner cwd {}", inner_cwd);

    let stderr_file =
        format!("{}/stderr/combined.real-stderr", outer_manifest_dir);
    let stderr_file_new = format!("{}.new", stderr_file);
    eprintln!("stderr file {}", &stderr_file);

    //Command::new("sh").args(["-xec", "env | sort"]).status().unwrap();

    fs::create_dir_all(&inner_cwd).unwrap();

    let command = env("CARGO").unwrap_or_else(|| "cargo".into());

    let mut args = vec![
        "check".into(),
        format!("--manifest-path={}/stderr/Cargo.toml", outer_manifest_dir),
        "--target-dir=target".into(),
        "--message-format=json".into(),
        "--quiet".into(),
        "--no-default-features".into(),
        "--features=enable".into(),
    ];

    let xoptions = [
        // Allows CI to pass --locked
        "STDERRTEST_CARGO_OPTIONS",
        // https://diziet.dreamwidth.org/tag/nailing-cargo
        "NAILINGCARGO_CARGO_OPTIONS",
    ]
    .iter()
    .cloned()
    .find_map(env);
    if let Some(xoptions) = xoptions {
        args.extend(xoptions.split_ascii_whitespace().map(Into::into))
    }

    eprint!("running {}", &command);
    for arg in &args {
        eprint!(" {}", &arg);
    }
    eprintln!();
    let mut command = Command::new(command);
    command.current_dir(&inner_cwd);
    command.stdout(Stdio::null());
    command.stderr(File::create(stderr_file_new).unwrap());
    command.args(args);

    let status = command.status().unwrap();

    assert!(!status.success());
    eprintln!("exit status: {} (error, as expected)", status);

    let mut scriptlet = Command::new("sh");
    scriptlet.arg("-ec");
    scriptlet.arg(
        r#"
        file="$1"; shift

        egrep -v '^error: could not compile `derive-adhoc-stderr-test' \
            "$file.new" >"$file.new.tmp"
        mv -v "$file.new.tmp" "$file.new"

        if diff -u -- "$file" "$file.new" >&2; then
            echo >&2 'stderr: no changes to stderr output.'
            rm -f "$file.new"
            exit 0
        fi

        case "$STDERRTEST" in
        overwrite)
            echo >&2 '*** changes to stderr output, installing! ***'
            mv -- "$file.new" "$file"
            ;;
        *)
            echo >&2 '*** changes to stderr output! ***'
            echo >&2 'set STDERRTEST=overwrite to declare it good'
            exit 1
            ;;
        esac
    "#,
    );
    scriptlet.args(["scriptlet", &stderr_file]);

    match scriptlet.status().unwrap() {
        status if status.code() == Some(0) => {}
        status if status.code() == Some(1) => process::exit(1),
        other => panic!("scriptlet crashed {}", other),
    }
}
