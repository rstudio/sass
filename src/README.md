# Updates and patches to `libsass`

## Updating

[libsass](https://github.com/sass/libsass) can be updated with the script `update_libsass.R`. The file contains the commit hash (in `master`) from which the source was cloned. The version number was obtained by running the `version.sh` shell script in a clean git checkout of libsass.

The `update_libsass.R` script should be run first, immediately followed by running the `apply_libsass_patches.R` script.

## Making a new patch

Here are the recommended steps for creating a new patch:

1. Make any necessary changes to files in `src`.
1. Create a patch with a command like `git diff {parent_commit_hash^} {child_commit_hash} > scripts/patches/[000]-a-description.patch`.
1. (Optional) Edit the patch file and add a comment to the top that explains what the patch does.
1. Add the new `.patch` file to the repo with a descriptive commit message.
1. Test your changes.

## Notes about compiled sources

- 001-remove-makefile-pipes.patch: The order-only prerequisite operators (|) were removed since they cannot be interpreted by Make 3.79.1 (which is used by Rtools 3.4). There were 3 instances
removed by this patch.

