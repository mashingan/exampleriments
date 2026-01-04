export const exs = {
	"jumat-01": 0,
	"sabtu-02": 0,
	"minggu-03": 0,
}

for (const [key, value] of Object.entries(exs)) {
	console.log(`${key}: ${value}`)
}

console.log(`total weekly: ${Object.values(exs).reduce((a, b) => a + b)}`)

