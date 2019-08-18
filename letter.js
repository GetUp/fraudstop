const APIMode = process.env.LIVE === 'true' ? 'LIVE' : 'TEST'
console.log({ APIMode })
const fs = require('fs')
const PDFDocument = require('pdfkit')
const moment = require('moment')

exports.handler = async (event) => {
  console.log({ event })
  const filename = await createLetter(event)
  const letter = encode64(filename)
  return {
    statusCode: 200,
    body: JSON.stringify({ letter }),
  }
}

function createLetter(details) {
  return new Promise((resolve, reject) => {
    const filename = `tmp/${details.crn}.pdf`
    let pendingStepCount = 2
    // both steps (stream close & doc.end) need to finish w/o error
    const stepFinished = () => { if (--pendingStepCount == 0) resolve(filename) }

    const writeStream = fs.createWriteStream(filename)
    writeStream.on('close', stepFinished)
    writeStream.on('error', reject)

    const doc = new PDFDocument({ size: 'A4' })
    doc.pipe(writeStream)
    writeLetter(doc, details)
    doc.end()
    stepFinished()
  })
}

function writeLetter(doc, d) {
  const fullAddress = `${d.address}, ${d.suburb} ${d.postcode}`
  const circumstances = d.personalCircumstances.filter(Boolean)

  doc.font('Helvetica-Bold')
    .text(moment().format('D MMM YYYY'))
    .moveDown()
    .text('To:')
    .text('Centrelink')
    .text('Reply Paid 7800')
    .text('Canberra BC ACT 2610')
    .moveDown()
    .text('Reply to:')
    .text(`${d.firstName} ${d.lastName}`)
    .text(d.address)
    .text(`${d.suburb}, ${d.postcode}`)
    .moveDown()

  doc.font('Helvetica')
    .text('I am writing to request a review by an Authorised Review Officer. My personal details, the decision I am appealing against, and my reasons for appealing are set out below.')
    .moveDown()
    .text(`Name: ${d.firstName} ${d.lastName}`)
    .text(`Date of birth: ${d.dob}`)
    .text(`Address: ${fullAddress}`)
    .text(`CRN: ${d.crn}`)
    .text(`Telephone: ${d.phone}`)
    .text(`Email: ${d.email}`)
    .moveDown()
    .text('I am appealing the decision to subject me to an automatically generated compliance intervention process.')
    .moveDown()
    .text('My reasons for appealing are:')
    .moveDown()
    .text(d.debtReason)
    .moveDown()
    .text('When investigating my case, I request that you address any errors you discover in my file, including but not limited to:')
    .moveDown()
    .text('• Any income, or source of income, which has been incorrectly duplicated (including but not limited to cases of the same employer having differently spelled or formatted names between my Centrelink records and ATO records)')
    .text('• Any income incorrectly calculated by virtue of being coded in multiple locations (including but not limited to lump sum payments, termination/leave payments, etc.)')
    .text('• Any non-assessable income incorrectly included with assessable income')
    .text('• Any income previously verified to be exempt from assessment by Centrelink staff that has since been incorrectly included with my assessable income')
    .text('• Any prior credits or arrears to me which were manually zeroed out by Centrelink staff for any reason and not paid to me, which have since been incorrectly included as payments to me')
    .text('• Any errors arising from the use of the Multical debt calculation tool')
    .text('• Any errors arising from Centrelink staff manually calculating my debt')
    .text('• Any evidence held by Centrelink (including but not limited to medical certificates, termination notices, notices concerning leave entitlements) concerning my income which have not been fully considered for the purposes of calculating my income')
    .text('• Any payslips provided by me or an employer which have not been correctly calculated')
    .text('• Any information whatsoever held by Centrelink regarding my income and payment types which has not been correctly considered or calculated')
    .text('• Any termination payments which were incorrectly included with my assessable income (including but not limited to the Age Pension)')
    .text('• Any recovery fees which have been incorrectly applied (including but not limited to any cases where a recovery fee has been applied without me being contacted by Centrelink staff to: assess me as not being vulnerable; explain my declaration requirements; and determine that my actions were deliberate and with no reasonable excuse)')
    .text('• Any instance where the ATO match data has been included in the incorrect Centrelink income category (including but not limited to cases where the full amount of income from ATO match data has been incorrectly included in my assessable income when none or only part of that income is properly assessable income)')
    .text('• Any cases where any paid parental leave has been incorrectly included as assessable income')
    .text('• Any instances where parts of my income from an employer (including but not limited to allowances which are wholly or partially exempt from being assessed as income for the purposes of Centrelink) have been incorrectly included as assessable income (including but not limited to instances where I have incorrectly reported wholly or partially exempt allowances or income as assessable income, or incorrectly accepted the inclusion of wholly or partially exempt allowances or income as assessable income).')
    .moveDown()

  if (circumstances.length) {
    doc.text('I also request that you take into consideration the following personal circumstance(s) which have impacted my ability to properly report income or caused me significant hardship:')
    circumstances.forEach(circumstance => doc.text(`• ${circumstance}`))
    doc.moveDown()
    doc.text('If you require further information (including documentary evidence) about the above personal circumstance(s), I request that you contact me by telephone to discuss it further. ')
    doc.moveDown()
  }

  doc.text('I request that any applicable recovery fees be waived. Further, I request that any penalty interest which may be applied to your debt claim against me be waived, and any penalty interest already collected be repaid.')
    .moveDown()
    .text('I also request that you suspend all recovery action against me, including any recovery action being conducted on your behalf by third party debt collectors, pending the outcome of my review.')
    .moveDown()

  doc.text('If you require further information from me or have any questions about the facts of my specific case, or if any aspects of my reasons for appealing which may be unclear, I request that you contact me by telephone to discuss it further.')
    .moveDown()
    .text('Furthermore, I request that you acknowledge this request for review by post upon receipt.')
    .moveDown()
    .text('Yours sincerely,')
    .text(`${d.firstName} ${d.lastName}`)
}

function encode64(file) {
  const data = fs.readFileSync(file)
  return new Buffer(data).toString('base64')
}
