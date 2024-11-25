import app, { port } from "./app";

app.listen(port, () => {
	console.log(`Server is listening on port ${port}`);
})