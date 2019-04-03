## Install xmonad and xmobar via stack

### Clone this repo to home directory (~)

### Get xmonad, xmonad-contrib, and xmobar
```
cd ~/.xmonad
git clone "https://github.com/xmonad/xmonad"
git clone "https://github.com/xmonad/xmonad-contrib"
git clone "https://github.com/jaor/xmobar"
```

### Build and install everything
`stack install`

### Recompile and restart xmonad
`xmonad --recompile && xmonad --restart`

### Remove this README.md :)
