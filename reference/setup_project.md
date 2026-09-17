# setup_project

Setup project

## Usage

``` r
setup_project(name, path = NULL, folders = NULL, git = TRUE, readme = TRUE)
```

## Arguments

- name:

  Name of project to create.

- path:

  Path where project is created.

- folders:

  Vector with names for folders to be created.

- git:

  Logical if git repo should be initialized.

- readme:

  Logical if README.md should be added.

## Value

void

## Details

Create project folder with certain structure.

## Examples

``` r
if (FALSE) setup_project(name = "Analysis_project", path = "~/Desktop") # \dontrun{}
```
