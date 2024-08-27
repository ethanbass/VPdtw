# VPdtw 2.2.1

This submission is a patch for the clang compiler warning pointed out by Professor Brian Ripley. I was given a deadline of August 28 to fix the warning. I also took this opportunity to make some other small updates mostly to the documentation.

- I have fixed the R CMD Check warning with the clang compiler by removing the offending line from the header and explicitly declaring the namespace for each function.

- I also resolved the "undeclared package" NOTE on the "fedora-clang" test system by removing the link to 'dtw'.

## Test environments

- Mac OS 12.6.8 (local machine, R 4.3.3)
- win-builder (release)
- win-builder (dev)
- linux (latest, rhub)
- macos (latest, rhub)
- macos-arm64 (latest, rhub)
- windows (latest, rhub)
- gcc14 (Fedora Linux 40, R-devel (2024-08-26 r87056), rhub)
- intel (Fedora Linux 38, R-devel (2024-08-26 r87056), rhub)
- mkl (Fedora Linux 38, R-devel (2024-08-26 r87056), rhub)
- ubuntu-clang (Ubuntu 22.04.4 LTS, R-devel (2024-08-26 r87056), rhub)
- ubuntu-gcc12 (Ubuntu 22.04.4 LTS, R-devel (2024-08-26 r87056), rhub)

## R CMD check results

0 errors | 0 warnings | 0 notes

