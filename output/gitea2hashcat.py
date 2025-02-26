import sqlite3
import base64
import sys

if len(sys.argv) != 2:
    print("Usage: python3 gitea3hashcat.py <gitea.db>")
    sys.exit(1)

try:
    con = sqlite3.connect(sys.argv[1])
    cursor = con.cursor()
    cursor.execute("SELECT name,passwd_hash_algo,salt,passwd FROM user")
    for row in cursor.fetchall():
        if "pbkdf2" in row[1]:
            algo, iterations, keylen = row[1].split("$")
            algo = "sha256"
            name = row[0]
        else:
            raise Exception("Unknown Algorithm")
        salt = bytes.fromhex(row[2])
        passwd = bytes.fromhex(row[3])
        salt_b64 = base64.b64encode(salt).decode("utf-8")
        passwd_b64 = base64.b64encode(passwd).decode("utf-8")
        print(f"{name}:{algo}:{iterations}:{salt_b64}:{passwd_b64}")
except Exception as e:
    print(f"Error: {e}")
    sys.exit(1)
