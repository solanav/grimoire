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

## Usage

Once you have Grimoire loaded, you can start using its utilities. First you should find an entry point to the system you are trying to exploit.

If you find a way of reading files in the remote server for example, you can create a function called `my-read-exploit` that reads files from the remote server.

After that, you can register your function with Grimoire:

```lisp
(grm/register :read 'my-read-exploit)
```

Grimoire now unlocks a bunch of functions that you can use to progress further, for example:

```lisp
* (grm/all-users)
'(root daemon bin sys sync games man _apt uuidd dnsmasq sshd pollinate ubuntu)

* (grm/system-info)
PRETTY_NAME="Ubuntu 20.04.2 LTS"
NAME="Ubuntu"
VERSION="20.04.2 LTS (Focal Fossa)"
ID=ubuntu
ID_LIKE=debian
HOME_URL="https://www.ubuntu.com/"
SUPPORT_URL="https://help.ubuntu.com/"
BUG_REPORT_URL="https://bugs.launchpad.net/ubuntu/"
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
