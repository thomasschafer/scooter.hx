use std::time::{Duration, Instant};
#[cfg(test)]
use std::{fs, path::Path};

#[cfg(test)]
macro_rules! create_test_files {
    () => {
        {
            use tempfile::TempDir;
            TempDir::new().unwrap()
        }
    };

    ($($name:expr => $content:expr),+ $(,)?) => {
        {
            use std::path::Path;
            use std::fs::{create_dir_all, File};
            use std::io::Write;
            use tempfile::TempDir;

            let temp_dir = TempDir::new().unwrap();

            $(
                let path = [temp_dir.path().to_str().unwrap(), $name].join("/");
                let path = Path::new(&path);
                create_dir_all(path.parent().unwrap()).unwrap();

                let mut file = File::create(path).unwrap();
                let content: &[u8] = $content;
                file.write_all(content).unwrap();
                file.sync_all().unwrap();
            )+

            #[cfg(windows)]
            {
                use std::time::Duration;
                std::thread::sleep(Duration::from_millis(100));
            }

            temp_dir
        }
    };
}

#[cfg(test)]
macro_rules! text {
    ($($line:expr),+ $(,)?) => {
        concat!($($line, "\n"),+).as_bytes()
    };
}

#[cfg(test)]
macro_rules! binary {
    ($($line:expr),+ $(,)?) => {{
        &[$($line, b"\n" as &[u8]),+].concat()
    }};
}

#[cfg(test)]
macro_rules! overwrite_files {
    ($base_dir:expr, $($name:expr => {$($line:expr),+ $(,)?}),+ $(,)?) => {
        {
            use std::fs::File;
            use std::io::Write;
            use std::path::Path;

            $(
                let contents = concat!($($line,"\n",)+);
                let path = Path::new($base_dir).join($name);

                if !path.exists() {
                    panic!("File does not exist: {}", path.display());
                }
                let mut file = File::create(&path).unwrap();
                file.write_all(contents.as_bytes()).unwrap();
                file.sync_all().unwrap();
            )+

            #[cfg(windows)]
            {
                use std::time::Duration;
                std::thread::sleep(Duration::from_millis(100));
            }
        }
    };
}

#[cfg(test)]
macro_rules! delete_files {
    ($base_dir:expr, $($path:expr),*) => {
        {
            use std::fs;
            use std::path::Path;
            $(
                let full_path = Path::new($base_dir).join($path);
                if !full_path.exists() {
                    panic!("Path does not exist: {}", full_path.display());
                }

                if full_path.is_dir() {
                    fs::remove_dir_all(&full_path).unwrap();
                } else {
                    fs::remove_file(&full_path).unwrap();
                }
            )*
        }
    };
}

#[cfg(test)]
pub fn collect_files(dir: &Path, base: &Path, files: &mut Vec<String>) {
    for entry in fs::read_dir(dir).unwrap() {
        let path = entry.unwrap().path();
        if path.is_file() {
            let rel_path = path
                .strip_prefix(base)
                .unwrap()
                .to_str()
                .unwrap()
                .to_string()
                .replace('\\', "/");
            files.push(rel_path);
        } else if path.is_dir() {
            collect_files(&path, base, files);
        }
    }
}

#[cfg(test)]
macro_rules! assert_test_files {
    ($temp_dir:expr) => {
        {
            let mut actual_files = Vec::new();
            $crate::test_utils::collect_files(
                $temp_dir.path(),
                $temp_dir.path(),
                &mut actual_files
            );

            assert!(
                actual_files.is_empty(),
                "Directory should be empty but contains files: {:?}",
                actual_files
            );
        }
    };

    ($temp_dir:expr, $($name:expr => $content:expr),+ $(,)?) => {
        {
            use std::fs;
            use std::path::Path;

            $(
                let expected_contents: &[u8] = $content;
                let path = Path::new($temp_dir.path()).join($name);

                assert!(path.exists(), "File {} does not exist", $name);

                let actual_contents = fs::read(&path)
                    .unwrap_or_else(|e| panic!("Failed to read file {}: {}", $name, e));

                #[allow(invalid_from_utf8)]
                if actual_contents != expected_contents {
                    assert_eq!(
                        actual_contents,
                        expected_contents,
                        "Contents mismatch for file {}\nExpected utf8 lossy conversion:\n{:?}\nActual utf8 lossy conversion:\n{:?}\n",
                        $name,
                        String::from_utf8_lossy(expected_contents),
                        String::from_utf8_lossy(&actual_contents),
                    );
                }
            )+

            let mut expected_files: Vec<String> = vec![$($name.to_string()),+];
            expected_files.sort();

            let mut actual_files = Vec::new();
            $crate::test_utils::collect_files(
                $temp_dir.path(),
                $temp_dir.path(),
                &mut actual_files
            );
            actual_files.sort();

            assert_eq!(
                actual_files,
                expected_files,
                "Directory contains unexpected files.\nExpected files: {:?}\nActual files: {:?}",
                expected_files,
                actual_files
            );
        }
    };
}

#[cfg(test)]
pub fn wait_until<F>(mut condition: F, timeout: Duration)
where
    F: FnMut() -> bool,
{
    let start = Instant::now();
    let poll_interval = Duration::from_millis(10);

    while start.elapsed() < timeout {
        if condition() {
            return;
        }
        std::thread::sleep(poll_interval);
    }

    panic!("Condition not met within {:?}", timeout)
}
