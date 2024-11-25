import express, { Request, Response } from "express";
import jwt from "jsonwebtoken";

const router = express.Router();

router.post("/login", (req: Request, res: Response) => {
    const token = jwt.sign(req.body, process.env.JWT_SECRET || "th1sk3y!s3ekr|t", {
        expiresIn: "1h",
    })
    res.status(200).json({ token })
})

export default router;