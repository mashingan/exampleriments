import { Request, Response, NextFunction } from "express";
import jwt from "jsonwebtoken";

interface JwtPayload {
    email: string,
}

export function checkJwtToken(req: Request, res: Response, next: NextFunction) {
    const token = req.headers.authorization?.split(" ")[1];
    if (!token) {
        return res.status(401).json({ message: "No token provided" })
    }

    try {
        const decoded = jwt.verify(token, process.env.JWT_SECRET || "th1sk3y!s3ekr|t") as JwtPayload;
        res.locals.email  = decoded.email;
        next();
    } catch(error) {
        return res.status(403).json({ message: "Invalid token" });
    }
}