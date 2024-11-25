import express, { Express, Request, Response } from "express";
import { createUser, listUser } from './userServiceSqlite';
import * as um from "./userServiceMysql";
import * as models from "./models";
import authRoute from "./authRoute";
import dotenv from "dotenv";

dotenv.config();

const app = express();
const port = process.env.PORT || 3000;

app.use(express.json());
app.use("/auth", authRoute);

interface RouteOp {
	registerPath: string,
	registerOp: (email: string, password: string) => Promise<models.User|string>,
	listPath: string,
	listOp: () => Promise<any>,
}
async function router(app: Express, dbsvc: RouteOp) {
	app.post(dbsvc.registerPath, async (req: Request, res: Response) => {
		const { email, password } = req.body;
		try {
			const user = await dbsvc.registerOp(email, password);
			res.status(201).json(user);
		} catch (err) {
			res.status(500).json({ error: err.message });
		}
	});
	app.get(dbsvc.listPath, async (req: Request, res: Response) => {
		try {
			const users = await dbsvc.listOp();
			res.status(200).json({ users });
		} catch (err) {
			res.status(500).json({ error: err.message });
		}
	});
}

router(app, {
	registerPath: "/register",
	registerOp: createUser,
	listPath: "/list",
	listOp: listUser,
});
router(app, {
	registerPath: "/mysql/register",
	registerOp: um.createUser,
	listPath: "/mysql/list",
	listOp: um.listUser,
});

app.get("/hello", async (req: Request, res: Response) => {
	res.status(200).json( { message: "Hello world" })
})

export default app;
export { port }