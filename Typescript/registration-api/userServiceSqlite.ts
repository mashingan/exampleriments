import { User } from "./models";
import { verbose } from "sqlite3";

const sq3 = verbose();
const db = new sq3.Database(":memory");

db.serialize(() => {
	db.run(`
CREATE TABLE IF NOT EXISTS users (
	id INTEGER PRIMARY KEY AUTOINCREMENT,
	email TEXT UNIQUE,
	password TEXT)
	`);
});

export async function createUser(email: string, password: string): Promise<User> {
	if (!email || !password) {
		throw new Error("Email and password are required.");
	}
	return new Promise<User>((resolve, reject) => {
		const stmt = db.prepare("INSERT INTO users(email, password) VALUES (?, ?)");
		stmt.run(email, password, function(err) {
			if (err) {
				console.log("Error inserting user:", err);
				reject(err);
			} else {
				console.log("Successfully inserting user:", this.lastID);
				resolve({ id: this.lastID, email, password });
			}
		});
		stmt.finalize();
	});
}

export async function listUser(): Promise<User[]> {
	return new Promise<User[]>((resolve, reject) => {
		const stmt = db.prepare("SELECT * FROM USERS");
		stmt.all((err, users) => {
			if (err) {
				console.log("Error fetching users:", err);
				reject(err);
			} else {
				console.log("Fetched user:", users);
				resolve( users as User[] );
			}
		});
	});
}