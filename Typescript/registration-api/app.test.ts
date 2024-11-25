import request from "supertest"
import app from "./app"

describe("GET /hello", () => {
    it("Responds with JSON message", async () => {
        const response = await request(app).get("/hello")
        expect(response.status).toBe(200)
        expect(response.body).toEqual({ message: "Hello world" })
    })
})