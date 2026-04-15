# Claude and Other Agents

# Guidelines

- NOTE: When the user's request matches an available skill:
    - ALWAYS invoke it using the Skill tool as your FIRST action.
    - Do NOT answer directly, do NOT use other tools first.
    - The skill has specialized workflows that produce better results than ad-hoc answers.

- CRITICAL: Always prefer the LSP tool over Grep/Read for code navigation.
    - Use it to find definitions, references, and workspace symbols.


- IMPORTANT: when planning and before you do any work:
  - ALWAYS mention how you would verify and validate that work is correct
  - include TDD tests in your plan
  - take a behaviour driven approach
  - you are very much ENCOURAGED to ask questions to get the design correct
  - ALWAYS seek clarifications to sort out ambiguities
  - ALWAYS provide a summary of the Design and implementation Plan


- NOTE: When the user asks for "second pass", "third pass" or "N-th pass" perform:
  - simplification opportunities,
  - naming/comments/docs quality review,
  - scan for edge-cases and logical regression,
  - in C/C++ NEVER produce undefined behavior and never segfault or stop executing without returning error or exceptions
  - all documentation up-to-date with changes,
  - running required formatter/lint/tests

- NOTE: when user asks for 'error handling' checks:
  - verify no panic in rust code
  - verify how errors are handled across-code base, all languages
  - ensure all errors handled and reported correctly with enough information reaching users

- NOTE: when user asks for 'edge cases':
  - look specifically edge cases
  - look for undefined behaviour or ambiguities
  - if necessary, ask the user to clarify

- NOTE: when user asks for 'code coverage':
    - explore all the code base looking for code that isn't yet tested.
    - Look specifically for testing edge cases.
    - Aim to have at least 95% test coverage.

- NOTE: When user asks for 'final prep' make:
    - final check everything builds, all languages and all tests pass
    - all examples in all languages compile and run
    - all docs build
    - if successful, carefully:
        - select files and contributions to git add
        - ignore the build files and artifacts, don't add hidden directories
        - if not in a branch, create a new properly named branch
        - git commit
        - make a pull request to upstream github project

- NOTE: when user asks to do 'pr reply' or 'pull request reply':
    - check github pull request reviews
    - consider them with respect to the philosophy and aims of this software
    - if in doubt seek user clarifications
    - fix code and address the raised issues
    - update the docs/
    - make a summary and push your changes to update the PR
    - poll to wait for the CI to finish running
    - continue iterating until all recommendations and issues were addressed

- NOTE: When user asks for 'make release' execute:
    - check all changes are committed and pushed upstream
    - final check everything builds, all languages and all tests pass
    - all examples in all languages compile and run
    - all docs build
    - if any of the above fails STOP and prompt the user for action
    - otherwise, proceed by check the latest version upstream and in VERSION file
    - if needed, bump in VERSION file, commit and tag then push to upstream
    - make a Github release

# Design & Purpose

- README.md -- entry level generic information
- docs/ -- full documentation in RST format

# Build / lint / test (required before marking done)

## Languages
This project contains C, C++, Fortran and Python code

# Version control
- Git project in github.com/ecmwf/multio
- Use this repository's own versioning and release processes.
- REMEMBER on releases:
    - check all is committed and pushed upstream, otherwise STOP and warn user
    - update the VERSION file
    - git tag with version
    - push and create release in github
