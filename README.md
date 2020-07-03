# vbump

Returns a the next semantic version based on the given list of git tags by extracting the latest version.

## Usage

vbump is used best in combination with git (orly) and its `tag` command. 

Given the following list of tags:
```
$>git tag -l
v1.0.1
v1.1.0
v1.1.1
v1.0.2
```

```
using `vbump` like this: 
$>git tag -l | vbump minor
v1.2.0
```

If no tags are present `0.1.0` is inferred as current version.