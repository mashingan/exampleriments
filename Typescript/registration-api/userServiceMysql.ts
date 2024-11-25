import { User } from "./models";
import { createPool, ResultSetHeader } from "mysql2";

const pool = createPool({
    host: "mysql",
    user: "root",
    password: "password",
    database: "mydb",
});

export async function createUser(email: string, password: string): Promise<User> {
    return new Promise((resolve, reject) => {
        pool.execute("INSERT INTO users(email, password) VALUES(?, ?)",
            [email, password],
            (err, result: ResultSetHeader) => {
                if (err) {
                    console.log("Error insert to mysql:", err);
                    reject(err);
                }
                console.log("the result is:", result);
                resolve({ id: result.insertId, email: email, password: null });
            });

    });
}

export async function listUser(): Promise<User[]> {
    return new Promise<User[]>((resolve, reject) => {
        pool.query("SELECT * FROM users", (err, result) => {
            if (err) {
                console.log("Error fetch list user:", err);
                reject(err)
            }
            console.log("result as:", result)
            resolve(result as User[])
        });
    });
}