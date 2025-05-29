# Grimoire

Grimoire is a collection of utilities designed to facilitate reconnaissance, exploitation, persistence, and privilege escalation.

It also includes tools to make the implementation of exploit POCs easier.

## Screenshots

![image](https://github.com/user-attachments/assets/ea321379-a96f-40e6-a285-7cf5c9af0955)

## Installation

To use Grimoire, clone the repository to your local machine.

```bash
git clone https://github.com/solanav/grimoire
```

After that, launch your common lisp environment (be it Lem, Emacs or any other IDE) and load Grimoire with:

```lisp
(ql:quickload :grimoire)
```
    
You should then enter the package and start working on the system itself, rather than using it from another package:
    
```lisp
(in-package :grimoire)
```
    
## Concepts

- **Capability**: Created using `define-capability`. They expose some type of operation to the framework such as reading files, blind code execution or file uploads.

- **Recipes**: Created using `define-recipe`. They use the capabilities available to do interesting or useful operations on the system. For example, the recipe `download-all` allows the user to download all files in a given remote path if the capacity to execute code is available to Grimoire.
    
- **Loot**: Managed through the function `loot`. You can save loot by passing a key and a value, retrieve it by passing a key only or listing the loot available by calling the function without parameters.

## Usage

Once you have Grimoire loaded, you can start using its utilities. First you should find an entry point to the system you are trying to exploit.

If you find a way of reading files in the remote server for example, you can create a function called `my-read-exploit` that reads files from the remote server:

```lisp
(define-capability :read my-read-exploit (file)
  (let* ((text (str:replace-all
                "\\x0A" (fmt "~%")
                (planning/query
                 (fmt "SELECT content FROM read_blob(\"~a\")"
                      file))))
         (text-len (1- (length text))))
    (if (plusp text-len)
        (subseq text 0 text-len)
        "")))
```

Grimoire now unlocks a bunch of functions that you can use to progress further, for example:

```lisp
* (all-users)
("root" "daemon" "bin" "sys" "sync" "games" "man" "lp" "mail"
 "news" "uucp" "proxy" "www-data" "backup" "list" "irc" "gnats"
 "nobody" "_apt" "grafana")

* (system-info)
(("PRETTY_NAME" . "Ubuntu 22.04.4 LTS")
 ("NAME" . "Ubuntu") 
 ("VERSION_ID" . "22.04")
 ("VERSION" . "22.04.4 LTS (Jammy Jellyfish)")
 ("VERSION_CODENAME" . "jammy") 
 ("ID" . "ubuntu")
 ("ID_LIKE" . "debian")
 ("HOME_URL" . "https://www.ubuntu.com/")
 ("SUPPORT_URL" . "https://help.ubuntu.com/")
 ("BUG_REPORT_URL" . "https://bugs.launchpad.net/ubuntu/")
 ("PRIVACY_POLICY_URL" . "https://.../privacy-policy")
 ("UBUNTU_CODENAME" . "jammy"))
```

## Roadmap

In no particular order:

- [ ] Package Grimoire with Lem so it can be used as a standalone tool.
- [ ] Add an easy way to create POCs and export them so Grimoire can use them.
- [ ] Add write utilities to Grimoire.
- [x] Add execute utilities to Grimoire.
- [ ] Add a more precise way of expressing capabilities and not just `:read`, `:write` and `:execute`.
- [ ] Add a graph like representation of capabilities and how they can be chained together.
- [ ] Add a way to easily share capabilities with other users.
- [ ] Add some kind of database to store loot and other stuff.
- [ ] Add logging.
- [ ] Add a way to easily create a report of the pentest.
- [ ] Add some testing maybe.
- [x] Add a deployable binary for persistance.
