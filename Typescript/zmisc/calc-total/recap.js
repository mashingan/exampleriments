/*
 *
 * Recap require 1 optional argument in format of {short month}-{year}
 * e.g. aug-2025, sep-2025 etc
 *
 * It default to current local date time for the month and year.
 * It's positioned on parent of years directories. E.g.
 * - recap.js
 * - 2025
 *   01-jan-01-07.js
 * - 2026
 *   01-jan-01-07.js
 *
 */
import fs from 'fs'

const monthNames = ["jan", "feb", "mar", "apr", "may", "jun",
"jul", "aug", "sep", "oct", "nov", "dec"];

(async () => {
	const thedate = Bun.argv.length >= 3 ?
		new Date(Date.parse(Bun.argv[2])) :
		new Date()

	const numMonth = thedate.getMonth()
	const theYear = thedate.getFullYear()
	const monthName = monthNames[numMonth]
	const numStr = `${String(numMonth+1).padStart(2, "0")}`
	const numMonthStr = `${numStr}-${monthName}-`
	const parentDir = `${__dirname}/${theYear}/`
	const exs = fs.readdirSync(parentDir, { withFileTypes: true }).
		filter(item => {
			return !item.isDirectory() &&
				item.name.startsWith(numStr) &&
				!item.name.includes("recap") &&
				item.name.endsWith(".js")
		}).
		map(item => item.name)
	let totalExpense = 0
	const fence = `${"-".repeat(10)}`
	for (const [idx, fname] of exs.entries()) {
		const rangeDate = fname.split(".")[0].split(numMonthStr)[1]
		const { exs } = await import(`${parentDir}/${fname}`)
		let weekly = 0
		for (const [_, expense] of Object.entries(exs ?? {})) {
			//console.log(`${date}: ${expense}`)
			weekly += expense
		}
		console.log(`week${idx+1}-${rangeDate}: ${weekly}`)
		console.log(`${fence}week${idx+1}${fence}`)
		totalExpense += weekly
	}
	console.log(`total monthly: ${totalExpense}`)
})()
