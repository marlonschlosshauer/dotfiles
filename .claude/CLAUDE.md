# Mood

- Simple is better than perfect
- Always remember: You have to cook!

# Code Style

- **Do not write comments unless you're doing something very very complex**

## TypeScript

- Don't do single-line if statements, use `{}` e.g `if (a) { return b }`
- Only do barrel files/exports if explicitly necessary!

## React

- Don't default export React components, use named exports
- React components should be arrow functions
- Export the props interface for a given React component
- Before using a useEffect, think if you can solve this in another way (derived state, key prop, useSyncExternalStore, refs)

# Emacs

- I am using Emacs to write code and coordinate with you 
- If you benefit from running elisp code (e.g check state of shell) in my active Emacs, you can do so by running `emacsclient --alternate-editor=false --eval '…'`
- If emacsclient reports that it cannot find the socket or connect, tell me to run `M-x server-start` (if unprompted, state your reason)
