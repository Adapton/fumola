//! Turning the path an `import` writes into the path a module is registered
//! under.
//!
//! A module's imports resolve relative to its own directory, so
//! `fumola/examples/mergeSort/mergeSort.fumola` writing `import A "adapton"`
//! means `fumola/examples/mergeSort/adapton`. Before this module existed the
//! joined string was used verbatim as a key, which made `..` meaningless: a
//! path that says `../../system/adapton` matched no registered module and the
//! import failed. The library worked around that with symlinks -- a copy of
//! every dependency in each directory that needed one -- which put files in
//! the listing that are not really there and made an import path a poor guide
//! to where its module actually lives.
//!
//! `normalize` resolves `.` and `..` the way a filesystem does, so the two
//! spellings of a path name the same module, and a reader can follow an import
//! path to the file.
//!
//! Deliberately lexical: there is no filesystem here (the wasm host has none
//! at all, and compiles the library in), so `..` is resolved by dropping the
//! preceding segment rather than by asking where a symlink points. For the
//! library that is the same answer, and it is the answer a reader gets from
//! the text alone.

/// Resolve `.` and `..` in a module path, and collapse empty segments.
///
/// A `..` that would climb above the root is kept, not dropped: for a local
/// path the host decides what the root is -- the CLI resolves an unregistered
/// path against the working directory, where a leading `..` is meaningful --
/// and for a package path it is an error worth reporting as written. Either
/// way, keeping it means a path that cannot be resolved is reported as the
/// caller spelled it rather than silently becoming a different module.
pub fn normalize(path: &str) -> String {
    let absolute = path.starts_with('/');
    let mut segments: Vec<&str> = Vec::new();
    for segment in path.split('/') {
        match segment {
            // "a//b" and a trailing "/" carry no meaning here.
            "" | "." => (),
            ".." => match segments.last() {
                // Climbing out of a leading "..", or out of an absolute
                // path's root, has nowhere to go: `/..` is `/`.
                Some(&"..") | None if !absolute => segments.push(".."),
                None => (),
                Some(_) => {
                    segments.pop();
                }
            },
            segment => segments.push(segment),
        }
    }
    let joined = segments.join("/");
    if absolute {
        format!("/{}", joined)
    } else {
        joined
    }
}

/// The directory a path lives in: everything before the last `/`, or the empty
/// string for a path with no `/` at all.
///
/// This is the prefix a module's own imports resolve against.
pub fn parent(path: &str) -> String {
    match path.rfind('/') {
        Some(index) => path[..index].to_string(),
        None => String::new(),
    }
}

/// Resolve `path` as written inside the module directory `dir`.
///
/// `dir` is `parent` of the importing module's path, or the empty string when
/// the importing module sits at the root.
pub fn join(dir: &str, path: &str) -> String {
    if dir.is_empty() {
        normalize(path)
    } else {
        normalize(&format!("{}/{}", dir, path))
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn a_plain_path_is_left_alone() {
        assert_eq!(
            normalize("fumola/collections/List"),
            "fumola/collections/List"
        );
        assert_eq!(normalize("List"), "List");
        assert_eq!(normalize(""), "");
    }

    #[test]
    fn dot_and_empty_segments_disappear() {
        assert_eq!(normalize("./List"), "List");
        assert_eq!(
            normalize("fumola/./collections/List"),
            "fumola/collections/List"
        );
        assert_eq!(
            normalize("fumola//collections/List"),
            "fumola/collections/List"
        );
        assert_eq!(normalize("fumola/collections/"), "fumola/collections");
    }

    #[test]
    fn dot_dot_drops_the_segment_before_it() {
        assert_eq!(
            normalize("fumola/collections/../system/adapton"),
            "fumola/system/adapton"
        );
        assert_eq!(
            normalize("fumola/examples/mergeSort/../../collections/List"),
            "fumola/collections/List"
        );
        assert_eq!(normalize("a/b/.."), "a");
        assert_eq!(normalize("a/.."), "");
    }

    /// The case the library needs: mergeSort reaching its two dependency
    /// directories, which is what the symlinks stood in for.
    #[test]
    fn mergesort_reaches_collections_and_system() {
        let dir = parent("fumola/examples/mergeSort/mergeSort");
        assert_eq!(dir, "fumola/examples/mergeSort");
        assert_eq!(
            join(&dir, "../../collections/levelTree"),
            "fumola/collections/levelTree"
        );
        assert_eq!(join(&dir, "../../system/adapton"), "fumola/system/adapton");
        assert_eq!(
            join(&dir, "mergeSort"),
            "fumola/examples/mergeSort/mergeSort"
        );
    }

    /// Kept rather than dropped, so an unresolvable path is reported as
    /// written -- and so the CLI can still find it on disk relative to the
    /// working directory.
    #[test]
    fn climbing_above_the_root_keeps_the_dot_dots() {
        assert_eq!(normalize("../List"), "../List");
        assert_eq!(normalize("../../List"), "../../List");
        assert_eq!(normalize("a/../../List"), "../List");
        assert_eq!(join("fumola", "../../List"), "../List");
    }

    /// An absolute path has a real root, and `/..` is `/` there, as in a
    /// filesystem.
    #[test]
    fn an_absolute_path_stays_absolute() {
        assert_eq!(
            normalize("/home/x/fumola/collections/List"),
            "/home/x/fumola/collections/List"
        );
        assert_eq!(normalize("/home/x/../List"), "/home/List");
        assert_eq!(normalize("/../List"), "/List");
    }

    #[test]
    fn parent_of_a_bare_name_is_empty() {
        assert_eq!(parent("List"), "");
        assert_eq!(parent("fumola/collections/List"), "fumola/collections");
        assert_eq!(parent("/fumola/List"), "/fumola");
    }

    #[test]
    fn join_from_the_root_is_just_normalize() {
        assert_eq!(join("", "List"), "List");
        assert_eq!(join("", "./List"), "List");
    }
}
