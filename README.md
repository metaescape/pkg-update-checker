# pkg-update-checker for Emacs

`pkg-update-checker` is an Emacs package designed to asynchronously check for package updates and notify the user through both an Emacs buffer and system notifications. This allows Emacs users to stay informed about available updates for their installed packages without interrupting their workflow. The package leverages the `async` package to ensure that the update check runs in the background, avoiding any disruption to the user's current tasks.

## Features

- **Asynchronous Update Checks**: Uses the `async` package to check for package updates without blocking Emacs.
- **User Notifications**: Notifies the user of available package updates through system notifications and an interactive buffer.
- **Customizable Check Interval**: Users can define how frequently (in hours) the update check should occur, with a default setting of every 24 hours.
- **Interactive Package List**: The upgradable packages are listed in a special Emacs buffer, where each package name is a clickable button that brings up the package description.

## Requirements

- Emacs 27.2 or higher
- async 1.9

## Installation

To install `pkg-update-checker`, clone this repository to your local machine and add the directory to your Emacs `load-path`. Then, require the package in your Emacs configuration:

```emacs-lisp
(add-to-list 'load-path "/path/to/pkg-update-checker")
(require 'pkg-update-checker)
```

Replace `"/path/to/pkg-update-checker"` with the actual path to where you have cloned or placed the `pkg-update-checker` package.

## Usage

To start the package updater timer, ensuring it only runs when Emacs is operating in server mode, add the following to your Emacs initialization file:

```emacs-lisp
(start-pkg-update-checker-timer)
```

This function checks if `server-mode` is active before starting the timer. If Emacs is not in server mode, it will output a message indicating that the package updater timer will not start.

use-package: 

```emacs-lisp
(use-package pkg-update-checker
  :defer 30
  :load-path "site-lisp/pkg-update-checker" 
  :config
  (start-package-updater-timer))
```

## Customization

You can customize the interval at which `pkg-update-checker` checks for updates by setting the `pkg-update-checker-interval-hour` variable in your Emacs configuration:

```emacs-lisp
(setq pkg-update-checker-interval-hour 12) ; Check for updates every 12 hours
```

## Misc
80% of the codes and README is Written by GPT-4
