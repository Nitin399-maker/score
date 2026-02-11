// ========== DATA MODEL ==========
let players = [];
let currentPlayerView = null;
let selectedComparePlayers = new Set();

// ========== UTILITY FUNCTIONS ==========
function formatDate(dateStr) {
    if (!dateStr || dateStr === 'N/A') return 'N/A';
    
    try {
        const date = new Date(dateStr);
        if (isNaN(date.getTime())) return dateStr;
        
        const months = ['Jan', 'Feb', 'Mar', 'Apr', 'May', 'Jun', 'Jul', 'Aug', 'Sep', 'Oct', 'Nov', 'Dec'];
        const day = date.getDate();
        
        return `${months[date.getMonth()]} ${day}, ${date.getFullYear()}`;
    } catch (e) {
        return dateStr;
    }
}

// ========== SCORING LOGIC ==========
function clamp(x, min, max) {
    return Math.max(min, Math.min(max, x));
}

function monthsBetween(dateStrA, dateStrB) {
    const a = new Date(dateStrA);
    const b = new Date(dateStrB);
    const months = (b.getFullYear() - a.getFullYear()) * 12 + (b.getMonth() - a.getMonth());
    return Math.max(0, months);
}

function decay(monthsAgo, halfLife) {
    if (halfLife <= 0) return 1;
    return Math.pow(0.5, monthsAgo / halfLife);
}

function sevWeight(sev) {
    switch ((sev || "").toLowerCase()) {
        case "major": return 9;
        case "moderate": return 5;
        case "minor": return 2;
        default: return 3;
    }
}

function typeMultiplier(type) {
    const t = (type || "").toLowerCase();
    if (t === "fracture") return 1.5;
    if (t === "dislocation" || t === "subluxation") return 1.4;
    if (t === "tear") return 1.3;
    if (t === "sprain") return 1.05;
    if (t === "strain") return 1.0;
    if (t === "tendinopathy") return 0.9;
    if (t === "contusion") return 0.6;
    return 1.0;
}

function procedureMultiplier(cat) {
    const c = (cat || "").toLowerCase();
    if (c === "reconstruction") return 1.25;
    if (c === "orif") return 1.35;
    if (c === "repair") return 1.15;
    if (c === "meniscectomy") return 1.0;
    if (c === "debridement") return 0.85;
    if (c === "tenex") return 0.75;
    return 1.0;
}

function residualPenalty(level) {
    switch ((level || "").toLowerCase()) {
        case "none": return 0;
        case "mild": return 2;
        case "moderate": return 5;
        case "severe": return 8;
        default: return 2;
    }
}

function limitationPenalty(lim) {
    switch ((lim || "").toLowerCase()) {
        case "none": return 0;
        case "weightroommods": return 2;
        case "brace": return 3;
        case "snapcount": return 4;
        default: return 1;
    }
}

function cartilagePenalty(level) {
    const v = (level || "").toLowerCase();
    if (v === "fullthickness") return 10;
    if (v === "severe") return 7;
    if (v === "moderate") return 4;
    if (v === "mild") return 2;
    return 0;
}

function degenerativePenalty(level) {
    const v = (level || "").toLowerCase();
    if (v === "severe") return 7;
    if (v === "moderate") return 4;
    if (v === "mild") return 2;
    return 0;
}

function labrumMeniscusPenalty(status) {
    const s = (status || "").toLowerCase();
    if (s === "confirmedretear") return 8;
    if (s === "possibleretear") return 5;
    return 0;
}

function tendonPenalty(status) {
    const s = (status || "").toLowerCase();
    if (s === "fulltear") return 9;
    if (s === "partialtear") return 6;
    if (s === "tendinosis") return 2;
    return 0;
}

function ligamentPenalty(status) {
    const s = (status || "").toLowerCase();
    if (s === "tear") return 8;
    if (s === "spraingrade2") return 4;
    if (s === "sprainlowgrade") return 2;
    return 0;
}

function effusionPenalty(level) {
    const e = (level || "").toLowerCase();
    if (e === "large") return 4;
    if (e === "moderate") return 2;
    if (e === "trace") return 1;
    return 0;
}

function buildChainIdForInjury(inj) {
    if (inj?.recurrenceGroupId) return `rg:${inj.recurrenceGroupId}`;
    const key = [
        inj?.bodyRegion || "Other",
        inj?.side || "NA",
        inj?.structure || "Unknown"
    ].join("|");
    return `inj:${key}`;
}

function buildChainIdForSurgery(surg) {
    if (surg?.reasonRelatedInjuryId) return `injId:${surg.reasonRelatedInjuryId}`;
    const key = [surg?.bodyRegion || "Other", surg?.side || "NA"].join("|");
    return `sx:${key}`;
}

function imagingChainKey(img) {
    return `img:${[img?.bodyRegion || "Other", img?.side || "NA"].join("|")}`;
}

function calculateMSI(facts, asOfDateStr) {
    const asOf = asOfDateStr || new Date().toISOString().slice(0, 10);

    const injuries = facts?.injuries || [];
    const surgeries = facts?.surgeries || [];
    const imgs = facts?.imagingFindings || [];
    const flags = facts?.flags || {};
    const counts = facts?.summaryCounts || {};
    const scoringInputs = facts?.scoringInputs || {};

    const chains = new Map();
    function ensure(chainId) {
        if (!chains.has(chainId)) chains.set(chainId, { injuryMax: 0, surgeryMax: 0, imagingMax: 0, incremental: 0 });
        return chains.get(chainId);
    }

    for (const inj of injuries) {
        const chainId = buildChainIdForInjury(inj);
        const c = ensure(chainId);

        const monthsAgo = inj?.date ? monthsBetween(inj.date, asOf) : 24;
        const hl = (inj?.severity === "Major") ? 48 : (inj?.severity === "Moderate" ? 30 : 18);
        let p = sevWeight(inj?.severity) * typeMultiplier(inj?.type) * decay(monthsAgo, hl);

        if (inj?.treatment?.surgery) p *= 0.35;
        if (inj?.recurrenceGroupId) c.incremental += 1.5 * decay(monthsAgo, 36);

        c.injuryMax = Math.max(c.injuryMax, p);
    }

    for (const sx of surgeries) {
        const chainId = sx?.reasonRelatedInjuryId ? `injId:${sx.reasonRelatedInjuryId}` : buildChainIdForSurgery(sx);
        const c = ensure(chainId);

        const monthsAgo = sx?.date ? monthsBetween(sx.date, asOf) : 60;
        const hl = sx?.majorJoint ? 72 : 60;

        const base = sx?.majorJoint ? 12 : 8;
        const proc = procedureMultiplier(sx?.procedureCategory);

        const revision = sx?.revision ? 6 : 0;
        const residual = residualPenalty(sx?.outcome?.residualSymptoms);
        const limitation = limitationPenalty(sx?.outcome?.currentLimitation);

        const p = (base * proc + revision + residual + limitation) * decay(monthsAgo, hl);

        c.surgeryMax = Math.max(c.surgeryMax, p);
        c.incremental += (revision + residual + limitation) * 0.35 * decay(monthsAgo, 72);
    }

    for (const img of imgs) {
        const chainId = imagingChainKey(img);
        const c = ensure(chainId);

        const monthsAgo = img?.date ? monthsBetween(img.date, asOf) : 24;

        const sf = img?.structuredFindings || {};
        const structural =
            (sf.nonunionOrDelayedUnion ? 10 : 0) +
            (sf.avascularNecrosisConcern ? 10 : 0) +
            (sf.hardwareComplication && sf.hardwareComplication !== "None" ? 6 : 0) +
            (sf.looseBodies ? 4 : 0) +
            (sf.stressReactionOrFracture ? 6 : 0);

        const structuralPart = structural * decay(monthsAgo, 84);

        const degenerativePart =
            (degenerativePenalty(sf.degenerativeChange) +
             cartilagePenalty(sf.cartilageDamage) +
             (sf.postTraumaticArthritis ? 6 : 0)) * decay(monthsAgo, 120);

        const softTissuePart =
            (labrumMeniscusPenalty(sf.labrumMeniscusStatus) +
             tendonPenalty(sf.tendonStatus) +
             ligamentPenalty(sf.ligamentStatus) +
             effusionPenalty(sf.effusion)) * decay(monthsAgo, 48);

        const p = structuralPart + degenerativePart + softTissuePart;
        c.imagingMax = Math.max(c.imagingMax, p);
    }

    let orthoPenalty = 0;
    for (const c of chains.values()) {
        const chainCore = Math.max(c.injuryMax, c.surgeryMax, c.imagingMax);
        orthoPenalty += chainCore + c.incremental;
    }

    let redFlagPenalty = 0;

    if (flags.fractureNonunionOrDelayedUnion) redFlagPenalty += 12;
    if (flags.avascularNecrosisConcern) redFlagPenalty += 12;
    if (flags.hardwareFailureOrBrokenImplant) redFlagPenalty += 10;

    if (flags.osteoarthritisOrArthrosis) redFlagPenalty += 8;
    if (flags.cartilageDegeneration) redFlagPenalty += 8;
    if (flags.looseBodies) redFlagPenalty += 5;

    if (flags.stressFractureHistory) redFlagPenalty += 6;

    if (flags.recurrentInstability) redFlagPenalty += 7;
    if (flags.recurrentMuscleStrain) redFlagPenalty += 4;

    redFlagPenalty += 3 * (scoringInputs.structuralRedFlagCount || 0);
    redFlagPenalty += 1.25 * (scoringInputs.degenerativeBurdenScore || 0);
    redFlagPenalty += 1.5 * (scoringInputs.instabilityBurdenScore || 0);

    const avail = facts?.availability || {};
    const bySeason = avail?.missedGamesBySeason || [];

    let missedGamesWeighted = 0;
    if (bySeason.length > 0) {
        for (const s of bySeason) {
            const yearsAgo = Math.max(0, (new Date(asOf).getFullYear() - (s.season || new Date(asOf).getFullYear())));
            const w = Math.pow(0.5, yearsAgo / 2.5);
            missedGamesWeighted += (s.missedGames || 0) * w;
        }
    } else {
        missedGamesWeighted = counts.missedGamesTotal || 0;
    }

    const availabilityPenalty =
        2.2 * Math.min(missedGamesWeighted, 8) +
        0.9 * Math.max(missedGamesWeighted - 8, 0) +
        0.8 * (avail.missedPracticeWeeksTotal || 0) +
        0.4 * (avail.limitedParticipationWeeksTotal || 0);

    const restr = (avail.currentRestrictions || "Unknown").toLowerCase();
    let restrictionPenalty = 0;
    if (restr === "limited") restrictionPenalty = 4;
    if (restr === "nocombine") restrictionPenalty = 7;
    if (restr === "prodayonly") restrictionPenalty = 5;

    const neuro = facts?.neuro || {};
    const concs = neuro?.concussions || [];
    const cerv = neuro?.cervicalEvents || [];

    let neuroPenalty = 0;

    for (const c of concs) {
        const monthsAgo = c?.date ? monthsBetween(c.date, asOf) : 36;
        let p = 6 * decay(monthsAgo, 36);

        if (c.lossOfConsciousness) p += 3 * decay(monthsAgo, 60);
        if (c.prolongedSymptoms) p += 4 * decay(monthsAgo, 60);

        p += 1.5 * (c.missedGames || 0) * decay(monthsAgo, 48);

        neuroPenalty += p;
    }

    const concCount = counts.concussionsTotal || concs.length;
    if (concCount >= 2) neuroPenalty += 5;
    if (concCount >= 3) neuroPenalty += 6;

    for (const e of cerv) {
        const monthsAgo = e?.date ? monthsBetween(e.date, asOf) : 36;
        let p = 6 * decay(monthsAgo, 48);

        if (e.recurrent) p += 4 * decay(monthsAgo, 72);
        if (e.currentSymptoms) p += 6;
        p += 1.5 * (e.timeLostGames || 0) * decay(monthsAgo, 48);

        neuroPenalty += p;
    }

    const mLast = scoringInputs.monthsSinceLastSignificantEvent ?? null;
    const monthsSinceLast = (mLast != null) ? mLast : 18;

    const recentBoost = clamp((12 - monthsSinceLast) / 12, 0, 1) * 0.35;

    const totalPenaltyBase =
        orthoPenalty +
        redFlagPenalty +
        availabilityPenalty +
        restrictionPenalty +
        neuroPenalty;

    const totalPenalty = totalPenaltyBase * (1 + recentBoost);

    const msi = Math.round(clamp(100 - totalPenalty, 0, 100));

    return {
        msi,
        breakdown: {
            orthoPenalty: +orthoPenalty.toFixed(1),
            redFlagPenalty: +redFlagPenalty.toFixed(1),
            availabilityPenalty: +(availabilityPenalty + restrictionPenalty).toFixed(1),
            neuroPenalty: +neuroPenalty.toFixed(1),
            recentBoostMultiplier: +(1 + recentBoost).toFixed(3),
            totalPenalty: +totalPenalty.toFixed(1)
        }
    };
}

function calculateScore(facts) {
    const result = calculateMSI(facts);
    return result.msi;
}

function getScoreLabel(score) {
    if (score >= 75) return { label: "Low Risk", class: "score-low", badge: "success" };
    if (score >= 50) return { label: "Medium Risk", class: "score-medium", badge: "warning" };
    return { label: "High Risk", class: "score-high", badge: "danger" };
}

function getScoreExplanation(breakdown) {
    if (!breakdown) return [];
    
    const deductions = [];
    if (breakdown.orthoPenalty > 0) deductions.push({ reason: "Orthopedic injuries & surgeries", value: breakdown.orthoPenalty });
    if (breakdown.redFlagPenalty > 0) deductions.push({ reason: "Structural red flags", value: breakdown.redFlagPenalty });
    if (breakdown.availabilityPenalty > 0) deductions.push({ reason: "Missed games & availability", value: breakdown.availabilityPenalty });
    if (breakdown.neuroPenalty > 0) deductions.push({ reason: "Neurological concerns", value: breakdown.neuroPenalty });
    
    deductions.sort((a, b) => b.value - a.value);
    return deductions.slice(0, 3);
}

// Recalculate all scores
function recalculateScores() {
    players.forEach(p => {
        const result = calculateMSI(p.facts);
        p.score = result.msi;
        p.scoreBreakdown = result.breakdown;
    });
}

recalculateScores();

// ========== PDF PROCESSING ==========
pdfjsLib.GlobalWorkerOptions.workerSrc = 'https://cdnjs.cloudflare.com/ajax/libs/pdf.js/3.11.174/pdf.worker.min.js';

async function extractTextFromPDF(file) {
    try {
        const arrayBuffer = await file.arrayBuffer();
        const pdf = await pdfjsLib.getDocument({ data: arrayBuffer }).promise;
        let fullText = '';
        
        for (let i = 1; i <= pdf.numPages; i++) {
            const page = await pdf.getPage(i);
            const textContent = await page.getTextContent();
            const pageText = textContent.items.map(item => item.str).join(' ');
            fullText += pageText + '\n';
        }
        
        return fullText;
    } catch (error) {
        console.error('Error extracting PDF text:', error);
        throw error;
    }
}

async function extractTextFromTXT(file) {
    return await file.text();
}

// ========== LLM INTEGRATION ==========
async function analyzeMedicalDocuments(documentsData, providedPlayerName = '') {
    const apiKey = document.getElementById('apiKey').value.trim();
    
    if (!apiKey) {
        throw new Error('Please enter your OpenAI API key');
    }

    // Combine all document texts
    const combinedDocuments = documentsData.map(doc => 
        `--- Document: ${doc.filename} ---\n${doc.text}\n`
    ).join('\n\n');

    const playerNameHint = providedPlayerName ? `Player Name (provided): ${providedPlayerName}` : 'Player Name: Extract from documents';

    const prompt = `You are a medical document analyzer for sports players. Analyze ALL the following medical documents Given by NFL for a single player and extract comprehensive medical information by combining data from all documents.

${playerNameHint}

Medical Documents:
${combinedDocuments}

Analyze ALL documents above and extract comprehensive information. Combine and merge data from all documents to create a complete medical profile.

Extract and return ONLY a valid JSON object with the following structure (no markdown, no code blocks, just raw JSON):
{
  "player": {
    "name": "string",
    "position": "QB|WR|RB|TE|OL|DL|LB|CB|S|K|P|LS|Unknown",
    "draftYear": 2022,
    "handedness": "L|R|Unknown"
  },
  "summaryCounts": {
    "surgeriesTotal": 0,
    "surgeriesMajorJoint": 0,
    "surgeriesNonMajorJoint": 0,
    "recurrenceTotal": 0,
    "missedGamesTotal": 0,
    "concussionsTotal": 0,
    "cervicalNeurologicEventsTotal": 0,
    "majorInjuriesTotal": 0,
    "moderateInjuriesTotal": 0,
    "minorInjuriesTotal": 0
  },
  "flags": {
    "cartilageDegeneration": false,
    "looseBodies": false,
    "effusionRecurrentOrModerate": false,
    "osteoarthritisOrArthrosis": false,
    "stressFractureHistory": false,
    "fractureNonunionOrDelayedUnion": false,
    "avascularNecrosisConcern": false,
    "hardwareFailureOrBrokenImplant": false,
    "recurrentInstability": false,
    "recurrentMuscleStrain": false
  },
  "availability": {
    "missedGamesBySeason": [
      { "season": 2022, "missedGames": 0, "reason": "string" }
    ],
    "missedPracticeWeeksTotal": 0,
    "limitedParticipationWeeksTotal": 0,
    "currentRestrictions": "None|Limited|NoCombine|ProDayOnly|Unknown",
    "availabilityNarrative": "string"
  },
  "injuries": [
    {
      "date": "YYYY-MM-DD",
      "season": 2024,
      "bodyRegion": "Head|CervicalSpine|Shoulder|Elbow|WristHand|HipGroin|ThighHamstring|Knee|AnkleFoot|LumbarSpine|Other",
      "structure": "string (e.g., MCL, labrum, meniscus)",
      "injuryName": "string",
      "type": "Sprain|Strain|Tear|Fracture|Dislocation|Subluxation|Tendinopathy|Contusion|Other",
      "side": "Left|Right|Bilateral|NA",
      "severity": "Major|Moderate|Minor",
      "mechanism": "Contact|NonContact|Overuse|Unknown",
      "recurrenceGroupId": "string-or-null",
      "treatment": {
        "surgery": false,
        "injection": "None|PRP|Cortisone|Other|Unknown",
        "braceOrTape": false,
        "rehabOnly": true
      },
      "timeLost": {
        "missedGames": 0,
        "missedPracticeWeeks": 0
      },
      "currentStatus": "Asymptomatic|Symptomatic|Recovered|Ongoing|Unknown",
      "notes": "string"
    }
  ],
  "surgeries": [
    {
      "date": "YYYY-MM-DD",
      "bodyRegion": "Shoulder|Knee|AnkleFoot|WristHand|HipGroin|LumbarSpine|Other",
      "procedure": "string",
      "procedureCategory": "Repair|Reconstruction|Debridement|Meniscectomy|ORIF|Tenex|Other",
      "side": "Left|Right|Bilateral|NA",
      "majorJoint": true,
      "revision": false,
      "reasonRelatedInjuryId": "optional reference",
      "outcome": {
        "returnedToPlay": true,
        "residualSymptoms": "None|Mild|Moderate|Severe|Unknown",
        "currentLimitation": "None|WeightRoomMods|Brace|SnapCount|Unknown"
      }
    }
  ],
  "imagingFindings": [
    {
      "date": "YYYY-MM-DD",
      "modality": "MRI|XR|CT|US|Other",
      "bodyRegion": "Shoulder|Knee|AnkleFoot|WristHand|HipGroin|LumbarSpine|CervicalSpine|Other",
      "side": "Left|Right|Bilateral|NA",
      "sourceDoc": "string",
      "structuredFindings": {
        "degenerativeChange": "None|Mild|Moderate|Severe|Unknown",
        "cartilageDamage": "None|Mild|Moderate|Severe|FullThickness|Unknown",
        "labrumMeniscusStatus": "Normal|PostOpNoRetear|PossibleRetear|ConfirmedRetear|Unknown",
        "tendonStatus": "Normal|Tendinosis|PartialTear|FullTear|Unknown",
        "ligamentStatus": "Normal|SprainLowGrade|SprainGrade2|Tear|ReconstructionIntact|Unknown",
        "effusion": "None|Trace|Moderate|Large|Unknown",
        "looseBodies": false,
        "nonunionOrDelayedUnion": false,
        "avascularNecrosisConcern": false,
        "hardwareComplication": "None|Lucency|Broken|Migration|Unknown",
        "postTraumaticArthritis": false,
        "stressReactionOrFracture": false
      },
      "imaging": {
        "finding": "finding description",
        "date": "YYYY-MM-DD",
        "doc": "source document name"
      }
    }
  ],
  "neuro": {
    "concussions": [
      {
        "date": "YYYY-MM-DD",
        "lossOfConsciousness": false,
        "timeLostDays": 0,
        "missedGames": 0,
        "prolongedSymptoms": false
      }
    ],
    "cervicalEvents": [
      {
        "date": "YYYY-MM-DD",
        "eventType": "Stinger|Radiculopathy|Neurapraxia|Other",
        "recurrent": false,
        "timeLostGames": 0,
        "currentSymptoms": false
      }
    ]
  },
  "timeline": [
    {"year": 2024, "event": "event description"}
  ],
  "scoringInputs": {
    "lastSignificantEventDate": "YYYY-MM-DD",
    "monthsSinceLastSignificantEvent": 0,
    "structuralRedFlagCount": 0,
    "degenerativeBurdenScore": 0,
    "instabilityBurdenScore": 0
  }
}

Important: 
- Combine and deduplicate information from all documents
- Return ONLY the JSON object, no additional text or formatting
- Ensure all arrays contain unique entries (no duplicates)
- Use the exact enum values specified (e.g., "Major" not "major")
- Fill in all required fields with best estimates from documents`;

    try {
        const response = await fetch('https://llmfoundry.straivedemo.com/openrouter/v1/chat/completions', {
            method: 'POST',
            headers: {
                'Content-Type': 'application/json',
                'Authorization': `Bearer ${apiKey}`
            },
            body: JSON.stringify({
                model: 'claude-4.5-sonnet',
                messages: [
                    {
                        role: 'system',
                        content: 'You are a medical document analyzer. Always respond with valid JSON only, no markdown formatting. Analyze multiple documents and combine the information into a single comprehensive medical profile.'
                    },
                    {
                        role: 'user',
                        content: prompt
                    }
                ]
            })
        });

        if (!response.ok) {
            const error = await response.json();
            throw new Error(error.error?.message || 'API request failed');
        }

        const data = await response.json();
        let content = data.choices[0].message.content.trim();
        
        // Remove markdown code blocks if present
        content = content.replace(/```json\n?/g, '').replace(/```\n?/g, '').trim();
        
        const analysis = JSON.parse(content);
        return analysis;
    } catch (error) {
        console.error('LLM Analysis Error:', error);
        throw error;
    }
}

function inferDocType(filename) {
    const lower = filename.toLowerCase();
    if (lower.includes('ortho')) return 'Ortho';
    if (lower.includes('genmed') || lower.includes('general')) return 'GenMed';
    if (lower.includes('mri')) return 'MRI';
    if (lower.includes('xr') || lower.includes('xray')) return 'XR';
    if (lower.includes('history')) return 'History';
    if (lower.includes('college')) return 'College';
    if (lower.includes('concussion')) return 'Concussion';
    if (lower.includes('knee')) return 'Knee';
    if (lower.includes('shoulder')) return 'Shoulder';
    return 'Medical';
}

// ========== UPLOAD & PROCESS ==========
document.getElementById('uploadFilesBtn').addEventListener('click', async () => {
    const files = document.getElementById('fileInput').files;
    const apiKey = document.getElementById('apiKey').value.trim();
    const providedPlayerName = document.getElementById('playerNameInput').value.trim();
    
    if (files.length === 0) {
        alert('Please select files to upload.');
        return;
    }
    
    if (!apiKey) {
        alert('Please enter your OpenAI API key.');
        return;
    }

    const progressContainer = document.getElementById('uploadProgress');
    const progressBar = progressContainer.querySelector('.progress-bar');
    const progressText = document.getElementById('progressText');
    
    progressContainer.classList.remove('d-none');
    document.getElementById('uploadFilesBtn').disabled = true;

    try {
        // Step 1: Extract text from all uploaded documents
        progressText.textContent = `Extracting text from ${files.length} document(s)...`;
        progressBar.style.width = '30%';
        
        const documentsData = [];
        for (const file of Array.from(files)) {
            let text;
            if (file.name.toLowerCase().endsWith('.pdf')) {
                text = await extractTextFromPDF(file);
            } else {
                text = await extractTextFromTXT(file);
            }
            
            documentsData.push({
                filename: file.name,
                docType: inferDocType(file.name),
                text: text
            });
        }

        // Step 2: Send all documents to LLM in a single request
        progressText.textContent = `Analyzing ${files.length} document(s) with AI...`;
        progressBar.style.width = '60%';
        
        const analysis = await analyzeMedicalDocuments(documentsData, providedPlayerName);

        // Step 3: Create or update player
        progressText.textContent = 'Creating player profile...';
        progressBar.style.width = '90%';
        
        const playerName = analysis.player?.name || providedPlayerName || 'Unknown Player';
        let player = players.find(p => p.name === playerName);

        if (!player) {
            // Create new player
            player = {
                id: players.length + 1,
                name: playerName,
                draftYear: analysis.player?.draftYear || 2022,
                handedness: analysis.player?.handedness || 'Unknown',
                documents: [],
                facts: analysis,
                score: 0,
                scoreBreakdown: null
            };
            players.push(player);
        } else {
            // Replace existing player data with new comprehensive analysis
            player.pos = analysis.player?.position || player.pos;
            player.draftYear = analysis.player?.draftYear || player.draftYear;
            player.handedness = analysis.player?.handedness || player.handedness;
            player.facts = analysis;
        }

        // Add all documents to player
        for (const docData of documentsData) {
            player.documents.push({
                filename: docData.filename,
                docType: docData.docType,
                uploadedAt: new Date().toISOString().split('T')[0]
            });
        }

        recalculateScores();
        renderPlayerSelector();
        renderCompareCheckboxes();
        document.getElementById('fileInput').value = '';
        document.getElementById('playerNameInput').value = '';
        
        progressText.textContent = 'Analysis complete!';
        progressBar.style.width = '100%';
        setTimeout(() => {
            progressContainer.classList.add('d-none');
            progressBar.style.width = '0%';
        }, 2000);
        
        showToast(`document(s) analyzed successfully for ${playerName}!`);

    } catch (error) {
        console.error('Upload error:', error);
        showToast(`Error: ${error.message}`, 'danger');
    } finally {
        document.getElementById('uploadFilesBtn').disabled = false;
    }
});

// ========== TAB B: PLAYER VIEW ==========
function renderPlayerSelector() {
    const selector = document.getElementById('playerSelector');
    selector.innerHTML = '<option value="">-- Select a Player --</option>';
    players.forEach(p => {
    const opt = document.createElement('option');
    opt.value = p.id;
    opt.textContent = `${p.name} (${p.pos})`;
    selector.appendChild(opt);
    });
}

document.getElementById('playerSelector').addEventListener('change', (e) => {
    const playerId = parseInt(e.target.value);
    if (!playerId) {
    document.getElementById('playerDashboard').innerHTML = '';
    return;
    }
    currentPlayerView = playerId;
    renderPlayerDashboard(playerId);
});

function renderPlayerDashboard(playerId) {
    const player = players.find(p => p.id === playerId);
    
    console.log('Rendering dashboard for:', player.name, 'Score:', player.score);
    
    const scoreInfo = getScoreLabel(player.score);
    const explanation = getScoreExplanation(player.scoreBreakdown);

    // Initialize sort state if not exists
    if (!player.sortState) {
        player.sortState = {
            injuries: { column: 'date', direction: 'desc' },
            surgeries: { column: 'date', direction: 'desc' }
        };
    }

    // Sort injuries
    const sortedInjuries = [...(player.facts.injuries || [])].sort((a, b) => {
        const state = player.sortState.injuries;
        let aVal, bVal;
        
        switch(state.column) {
            case 'date':
                aVal = new Date(a.date || '1900-01-01');
                bVal = new Date(b.date || '1900-01-01');
                break;
            case 'injury':
                aVal = (a.injuryName || 'Unknown').toLowerCase();
                bVal = (b.injuryName || 'Unknown').toLowerCase();
                break;
            case 'bodyRegion':
                aVal = (a.bodyRegion || 'Unknown').toLowerCase();
                bVal = (b.bodyRegion || 'Unknown').toLowerCase();
                break;
            case 'severity':
                const sevOrder = { 'Major': 3, 'Moderate': 2, 'Minor': 1, 'Unknown': 0 };
                aVal = sevOrder[a.severity] || 0;
                bVal = sevOrder[b.severity] || 0;
                break;
            case 'status':
                aVal = (a.currentStatus || 'Unknown').toLowerCase();
                bVal = (b.currentStatus || 'Unknown').toLowerCase();
                break;
            default:
                return 0;
        }
        
        if (aVal < bVal) return state.direction === 'asc' ? -1 : 1;
        if (aVal > bVal) return state.direction === 'asc' ? 1 : -1;
        return 0;
    });

    // Sort surgeries
    const sortedSurgeries = [...(player.facts.surgeries || [])].sort((a, b) => {
        const state = player.sortState.surgeries;
        let aVal, bVal;
        
        switch(state.column) {
            case 'date':
                aVal = new Date(a.date || '1900-01-01');
                bVal = new Date(b.date || '1900-01-01');
                break;
            case 'procedure':
                aVal = (a.procedure || 'Unknown').toLowerCase();
                bVal = (b.procedure || 'Unknown').toLowerCase();
                break;
            case 'bodyRegion':
                aVal = (a.bodyRegion || 'Unknown').toLowerCase();
                bVal = (b.bodyRegion || 'Unknown').toLowerCase();
                break;
            case 'type':
                aVal = (a.procedureCategory || 'Unknown').toLowerCase();
                bVal = (b.procedureCategory || 'Unknown').toLowerCase();
                break;
            case 'outcome':
                aVal = (a.outcome?.residualSymptoms || 'Unknown').toLowerCase();
                bVal = (b.outcome?.residualSymptoms || 'Unknown').toLowerCase();
                break;
            default:
                return 0;
        }
        
        if (aVal < bVal) return state.direction === 'asc' ? -1 : 1;
        if (aVal > bVal) return state.direction === 'asc' ? 1 : -1;
        return 0;
    });

    const dashboard = document.getElementById('playerDashboard');
    dashboard.innerHTML = `
    <div class="card mb-3">
        <div class="card-body">
        <div class="d-flex justify-content-between align-items-center mb-3">
            <h4>${player.name} <span class="badge bg-secondary">${player.pos}</span></h4>
            <button class="btn btn-sm btn-outline-secondary d-none" onclick="openEditFactsModal(${player.id})">
                <i class="bi bi-pencil me-1"></i> Edit Facts
            </button>

        </div>
        <div class="row">
            <div class="col-md-4 text-center">
            <div class="score-circle ${scoreInfo.class}">${player.score}</div>
            <h5 class="mt-3"><span class="badge bg-${scoreInfo.badge}">${scoreInfo.label}</span></h5>
            <div class="progress mt-2" style="height: 25px;">
                <div class="progress-bar bg-${scoreInfo.badge}" role="progressbar" style="width: ${player.score}%">${player.score}%</div>
            </div>
            </div>
            <div class="col-md-8">
            <h6>Score Explanation</h6>
            <ul class="list-unstyled">
                ${explanation.length > 0 ? explanation.map(e => `<li><i class="bi bi-dash-circle text-danger me-1"></i> <strong>-${e.value.toFixed(1)} points:</strong> ${e.reason}</li>`).join('') : '<li class="text-muted">No deductions</li>'}
            </ul>
            ${player.scoreBreakdown ? `
                <div class="mt-3">
                    <small class="text-muted">
                        <strong>Total Penalty:</strong> ${player.scoreBreakdown.totalPenalty.toFixed(1)} points<br>
                        <strong>Recent Boost:</strong> ${player.scoreBreakdown.recentBoostMultiplier}x
                    </small>
                </div>
            ` : ''}
            </div>
        </div>
        </div>
    </div>

    <div class="card mb-3">
        <div class="card-header"><h5>Critical Information</h5></div>
        <div class="card-body">
        <h6>Injuries</h6>
        <table class="table table-sm table-striped">
            <thead><tr>
                <th class="sortable-header" onclick="sortPlayerTable(${playerId}, 'injuries', 'injury')" style="cursor: pointer;">
                    Injury ${player.sortState.injuries.column === 'injury' ? (player.sortState.injuries.direction === 'asc' ? '▲' : '▼') : ''}
                </th>
                <th class="sortable-header" onclick="sortPlayerTable(${playerId}, 'injuries', 'bodyRegion')" style="cursor: pointer;">
                    Body Region ${player.sortState.injuries.column === 'bodyRegion' ? (player.sortState.injuries.direction === 'asc' ? '▲' : '▼') : ''}
                </th>
                <th class="sortable-header" onclick="sortPlayerTable(${playerId}, 'injuries', 'date')" style="cursor: pointer;">
                    Date ${player.sortState.injuries.column === 'date' ? (player.sortState.injuries.direction === 'asc' ? '▲' : '▼') : ''}
                </th>
                <th class="sortable-header" onclick="sortPlayerTable(${playerId}, 'injuries', 'severity')" style="cursor: pointer;">
                    Severity ${player.sortState.injuries.column === 'severity' ? (player.sortState.injuries.direction === 'asc' ? '▲' : '▼') : ''}
                </th>
                <th class="sortable-header" onclick="sortPlayerTable(${playerId}, 'injuries', 'status')" style="cursor: pointer;">
                    Status ${player.sortState.injuries.column === 'status' ? (player.sortState.injuries.direction === 'asc' ? '▲' : '▼') : ''}
                </th>
            </tr></thead>
            <tbody>
            ${sortedInjuries.map((inj, i) => `
                <tr>
                <td>${inj.injuryName || 'Unknown'}</td>
                <td>${inj.bodyRegion || 'Unknown'} ${inj.side !== 'NA' ? `(${inj.side})` : ''}</td>
                <td>${formatDate(inj.date)}</td>
                <td><span class="badge bg-${inj.severity === 'Major' ? 'danger' : inj.severity === 'Moderate' ? 'warning' : 'secondary'}">${inj.severity || 'Unknown'}</span></td>
                <td><span class="badge bg-${inj.currentStatus === 'Recovered' || inj.currentStatus === 'Asymptomatic' ? 'success' : 'warning'}">${inj.currentStatus || 'Unknown'}</span></td>
                </tr>
            `).join('')}
            </tbody>
        </table>

        <h6 class="mt-3">Surgeries / Procedures</h6>
        <table class="table table-sm table-striped">
            <thead><tr>
                <th class="sortable-header" onclick="sortPlayerTable(${playerId}, 'surgeries', 'procedure')" style="cursor: pointer;">
                    Procedure ${player.sortState.surgeries.column === 'procedure' ? (player.sortState.surgeries.direction === 'asc' ? '▲' : '▼') : ''}
                </th>
                <th class="sortable-header" onclick="sortPlayerTable(${playerId}, 'surgeries', 'bodyRegion')" style="cursor: pointer;">
                    Body Region ${player.sortState.surgeries.column === 'bodyRegion' ? (player.sortState.surgeries.direction === 'asc' ? '▲' : '▼') : ''}
                </th>
                <th class="sortable-header" onclick="sortPlayerTable(${playerId}, 'surgeries', 'date')" style="cursor: pointer;">
                    Date ${player.sortState.surgeries.column === 'date' ? (player.sortState.surgeries.direction === 'asc' ? '▲' : '▼') : ''}
                </th>
                <th class="sortable-header" onclick="sortPlayerTable(${playerId}, 'surgeries', 'type')" style="cursor: pointer;">
                    Type ${player.sortState.surgeries.column === 'type' ? (player.sortState.surgeries.direction === 'asc' ? '▲' : '▼') : ''}
                </th>
                <th class="sortable-header" onclick="sortPlayerTable(${playerId}, 'surgeries', 'outcome')" style="cursor: pointer;">
                    Outcome ${player.sortState.surgeries.column === 'outcome' ? (player.sortState.surgeries.direction === 'asc' ? '▲' : '▼') : ''}
                </th>
            </tr></thead>
            <tbody>
            ${sortedSurgeries.map(surg => `
                <tr>
                <td>${surg.procedure || 'Unknown'}</td>
                <td>${surg.bodyRegion || 'Unknown'} ${surg.side !== 'NA' ? `(${surg.side})` : ''}</td>
                <td>${formatDate(surg.date)}</td>
                <td><span class="badge bg-${surg.majorJoint ? 'danger' : 'info'}">${surg.procedureCategory || 'Unknown'}</span></td>
                <td><span class="badge bg-${surg.outcome?.returnedToPlay ? 'success' : 'warning'}">${surg.outcome?.residualSymptoms || 'Unknown'}</span></td>
                </tr>
            `).join('')}
            </tbody>
        </table>

        <h6 class="mt-3">Imaging Findings</h6>
        <div class="accordion" id="imagingAccordion">
            ${(player.facts.imagingFindings || []).map((img, i) => `
            <div class="accordion-item">
                <h2 class="accordion-header">
                <button class="accordion-button collapsed" type="button" data-bs-toggle="collapse" data-bs-target="#img${i}">
                    ${img.modality || 'Imaging'} - ${img.bodyRegion || 'Unknown'} (${formatDate(img.date)})
                </button>
                </h2>
                <div id="img${i}" class="accordion-collapse collapse" data-bs-parent="#imagingAccordion">
                <div class="accordion-body">
                    <strong>Modality:</strong> ${img.modality || 'Unknown'}<br>
                    <strong>Body Region:</strong> ${img.bodyRegion || 'Unknown'} ${img.side !== 'NA' ? `(${img.side})` : ''}<br>
                    <strong>Date:</strong> ${formatDate(img.date)}<br>
                    <strong>Source:</strong> ${img.sourceDoc || 'Unknown'}<br>
                    ${img.imaging?.finding ? `
                        <hr>
                        <div class="alert alert-info mb-2">
                            <strong><i class="bi bi-file-medical me-1"></i>Finding Description:</strong><br>
                            ${img.imaging.finding}
                        </div>
                    ` : ''}
                    ${img.structuredFindings ? `
                        <hr>
                        <strong>Structured Findings:</strong><br>
                        ${img.structuredFindings.degenerativeChange && img.structuredFindings.degenerativeChange !== 'None' && img.structuredFindings.degenerativeChange !== 'Unknown' ? `• Degenerative Change: <span class="badge bg-warning">${img.structuredFindings.degenerativeChange}</span><br>` : ''}
                        ${img.structuredFindings.cartilageDamage && img.structuredFindings.cartilageDamage !== 'None' && img.structuredFindings.cartilageDamage !== 'Unknown' ? `• Cartilage Damage: <span class="badge bg-warning">${img.structuredFindings.cartilageDamage}</span><br>` : ''}
                        ${img.structuredFindings.labrumMeniscusStatus && img.structuredFindings.labrumMeniscusStatus !== 'Normal' && img.structuredFindings.labrumMeniscusStatus !== 'Unknown' ? `• Labrum/Meniscus: <span class="badge bg-warning">${img.structuredFindings.labrumMeniscusStatus}</span><br>` : ''}
                        ${img.structuredFindings.tendonStatus && img.structuredFindings.tendonStatus !== 'Normal' && img.structuredFindings.tendonStatus !== 'Unknown' ? `• Tendon: <span class="badge bg-warning">${img.structuredFindings.tendonStatus}</span><br>` : ''}
                        ${img.structuredFindings.ligamentStatus && img.structuredFindings.ligamentStatus !== 'Normal' && img.structuredFindings.ligamentStatus !== 'Unknown' ? `• Ligament: <span class="badge bg-warning">${img.structuredFindings.ligamentStatus}</span><br>` : ''}
                        ${img.structuredFindings.effusion && img.structuredFindings.effusion !== 'None' && img.structuredFindings.effusion !== 'Unknown' ? `• Effusion: <span class="badge bg-info">${img.structuredFindings.effusion}</span><br>` : ''}
                        ${img.structuredFindings.looseBodies ? `• <span class="badge bg-danger">Loose Bodies Present</span><br>` : ''}
                        ${img.structuredFindings.nonunionOrDelayedUnion ? `• <span class="badge bg-danger">Nonunion/Delayed Union</span><br>` : ''}
                        ${img.structuredFindings.avascularNecrosisConcern ? `• <span class="badge bg-danger">AVN Concern</span><br>` : ''}
                        ${img.structuredFindings.postTraumaticArthritis ? `• <span class="badge bg-danger">Post-Traumatic Arthritis</span><br>` : ''}
                        ${img.structuredFindings.stressReactionOrFracture ? `• <span class="badge bg-danger">Stress Reaction/Fracture</span><br>` : ''}
                        ${img.structuredFindings.hardwareComplication && img.structuredFindings.hardwareComplication !== 'None' && img.structuredFindings.hardwareComplication !== 'Unknown' ? `• Hardware: <span class="badge bg-warning">${img.structuredFindings.hardwareComplication}</span><br>` : ''}
                    ` : ''}
                </div>
                </div>
            </div>
            `).join('')}
        </div>

        <h6 class="mt-3">Missed Time / Availability</h6>
        <p>${player.facts.availability?.availabilityNarrative || 'No data'}</p>
        ${(player.facts.availability?.missedGamesBySeason || []).length > 0 ? `
            <table class="table table-sm">
                <thead><tr><th>Season</th><th>Missed Games</th><th>Reason</th></tr></thead>
                <tbody>
                ${player.facts.availability.missedGamesBySeason.map(s => `
                    <tr><td>${s.season}</td><td>${s.missedGames}</td><td>${s.reason || 'N/A'}</td></tr>
                `).join('')}
                </tbody>
            </table>
        ` : ''}

        <h6 class="mt-3">Medical Flags</h6>
        <div>
            ${player.facts.flags?.cartilageDegeneration ? '<span class="badge bg-danger me-1">Cartilage Degeneration</span>' : ''}
            ${player.facts.flags?.looseBodies ? '<span class="badge bg-danger me-1">Loose Bodies</span>' : ''}
            ${player.facts.flags?.osteoarthritisOrArthrosis ? '<span class="badge bg-danger me-1">Osteoarthritis</span>' : ''}
            ${player.facts.flags?.recurrentInstability ? '<span class="badge bg-warning me-1">Recurrent Instability</span>' : ''}
            ${player.facts.flags?.stressFractureHistory ? '<span class="badge bg-warning me-1">Stress Fracture History</span>' : ''}
            ${player.facts.summaryCounts?.recurrenceTotal > 0 ? `<span class="badge bg-warning text-dark me-1">${player.facts.summaryCounts.recurrenceTotal} Recurrences</span>` : ''}
            ${!player.facts.flags?.cartilageDegeneration && !player.facts.flags?.looseBodies && !player.facts.flags?.osteoarthritisOrArthrosis && !player.facts.flags?.recurrentInstability ? '<span class="badge bg-success">No Major Flags</span>' : ''}
        </div>
        </div>
    </div>

    <div class="card mb-3">
        <div class="card-header"><h5>Medical Timeline</h5></div>
        <div class="card-body">
        ${(() => {
            // Build comprehensive timeline from injuries, surgeries, and missed games
            const timelineEvents = [];
            
            // Add injuries
            (player.facts.injuries || []).forEach(inj => {
                if (inj.date) {
                    timelineEvents.push({
                        date: inj.date,
                        type: 'injury',
                        icon: 'bi-bandaid-fill',
                        color: 'danger',
                        title: inj.injuryName || 'Injury',
                        details: `${inj.bodyRegion || 'Unknown'} ${inj.side !== 'NA' ? `(${inj.side})` : ''} - ${inj.severity || 'Unknown'} ${inj.type || ''}`,
                        missedGames: inj.timeLost?.missedGames || 0
                    });
                }
            });
            
            // Add surgeries
            (player.facts.surgeries || []).forEach(surg => {
                if (surg.date) {
                    timelineEvents.push({
                        date: surg.date,
                        type: 'surgery',
                        icon: 'bi-scissors',
                        color: 'primary',
                        title: surg.procedure || 'Surgery',
                        details: `${surg.bodyRegion || 'Unknown'} ${surg.side !== 'NA' ? `(${surg.side})` : ''} - ${surg.procedureCategory || 'Unknown'}`,
                        outcome: surg.outcome?.residualSymptoms || 'Unknown'
                    });
                }
            });
            
            // Add missed games by season
            (player.facts.availability?.missedGamesBySeason || []).forEach(season => {
                if (season.missedGames > 0) {
                    timelineEvents.push({
                        date: `${season.season}-09-01`, // Approximate season start
                        type: 'missed',
                        icon: 'bi-calendar-x',
                        color: 'warning',
                        title: `${season.season} Season`,
                        details: `Missed ${season.missedGames} game(s)`,
                        reason: season.reason || 'Not specified'
                    });
                }
            });
            
            // Sort by date (most recent first)
            timelineEvents.sort((a, b) => new Date(b.date) - new Date(a.date));
            
            if (timelineEvents.length === 0) {
                return '<p class="text-muted">No timeline data available</p>';
            }
            
            return `
                <div class="timeline">
                    ${timelineEvents.map(event => `
                        <div class="timeline-item mb-3 pb-3 border-bottom">
                            <div class="d-flex align-items-start">
                                <div class="me-3">
                                    <i class="bi ${event.icon} text-${event.color} fs-4"></i>
                                </div>
                                <div class="flex-grow-1">
                                    <div class="d-flex justify-content-between align-items-start">
                                        <div>
                                            <h6 class="mb-1">
                                                <span class="badge bg-${event.color} me-2">${event.type.toUpperCase()}</span>
                                                ${event.title}
                                            </h6>
                                            <p class="mb-1 text-muted small">${formatDate(event.date)}</p>
                                            <p class="mb-1">${event.details}</p>
                                            ${event.missedGames ? `<small class="text-danger"><i class="bi bi-exclamation-circle me-1"></i>Missed ${event.missedGames} game(s)</small>` : ''}
                                            ${event.reason ? `<small class="text-muted d-block">Reason: ${event.reason}</small>` : ''}
                                            ${event.outcome ? `<small class="text-muted d-block">Outcome: ${event.outcome}</small>` : ''}
                                        </div>
                                    </div>
                                </div>
                            </div>
                        </div>
                    `).join('')}
                </div>
            `;
        })()}
        </div>
    </div>
    `;
}

function sortPlayerTable(playerId, tableType, column) {
    const player = players.find(p => p.id === playerId);
    if (!player || !player.sortState) return;
    
    const state = player.sortState[tableType];
    
    // Toggle direction if same column, otherwise set to descending
    if (state.column === column) {
        state.direction = state.direction === 'asc' ? 'desc' : 'asc';
    } else {
        state.column = column;
        state.direction = 'desc';
    }
    
    // Re-render the dashboard
    renderPlayerDashboard(playerId);
}

function openEditFactsModal(playerId) {
    currentPlayerView = playerId;
    const player = players.find(p => p.id === playerId);
    const counts = player.facts.summaryCounts || {};
    
    document.getElementById('editSurgeries').value = counts.surgeriesTotal || 0;
    document.getElementById('editMajorJoint').value = counts.surgeriesMajorJoint || 0;
    document.getElementById('editRecurrence').value = counts.recurrenceTotal || 0;
    document.getElementById('editMissedGames').value = counts.missedGamesTotal || 0;
    document.getElementById('editConcussions').value = counts.concussionsTotal || 0;
    document.getElementById('editCartilage').checked = player.facts.flags?.cartilageDegeneration || false;
    document.getElementById('editLooseBodies').checked = player.facts.flags?.looseBodies || false;
    document.getElementById('editCervical').checked = (counts.cervicalNeurologicEventsTotal || 0) > 0;

    new bootstrap.Modal(document.getElementById('editFactsModal')).show();
}

document.getElementById('saveFactsBtn').addEventListener('click', () => {
    const player = players.find(p => p.id === currentPlayerView);
    if (!player.facts.summaryCounts) player.facts.summaryCounts = {};
    if (!player.facts.flags) player.facts.flags = {};
    if (!player.facts.availability) player.facts.availability = {};
    if (!player.facts.scoringInputs) player.facts.scoringInputs = {};
    
    // Update counts
    player.facts.summaryCounts.surgeriesTotal = parseInt(document.getElementById('editSurgeries').value);
    player.facts.summaryCounts.surgeriesMajorJoint = parseInt(document.getElementById('editMajorJoint').value);
    player.facts.summaryCounts.recurrenceTotal = parseInt(document.getElementById('editRecurrence').value);
    player.facts.summaryCounts.missedGamesTotal = parseInt(document.getElementById('editMissedGames').value);
    player.facts.summaryCounts.concussionsTotal = parseInt(document.getElementById('editConcussions').value);
    
    // Update flags
    player.facts.flags.cartilageDegeneration = document.getElementById('editCartilage').checked;
    player.facts.flags.looseBodies = document.getElementById('editLooseBodies').checked;
    player.facts.summaryCounts.cervicalNeurologicEventsTotal = document.getElementById('editCervical').checked ? 1 : 0;
    
    // Update scoring inputs for flags
    const structuralFlags = 
        (player.facts.flags.fractureNonunionOrDelayedUnion ? 1 : 0) +
        (player.facts.flags.avascularNecrosisConcern ? 1 : 0) +
        (player.facts.flags.hardwareFailureOrBrokenImplant ? 1 : 0) +
        (player.facts.flags.looseBodies ? 1 : 0);
    
    const degenerativeScore = 
        (player.facts.flags.osteoarthritisOrArthrosis ? 3 : 0) +
        (player.facts.flags.cartilageDegeneration ? 3 : 0);
    
    const instabilityScore = 
        (player.facts.flags.recurrentInstability ? 3 : 0) +
        (player.facts.flags.recurrentMuscleStrain ? 2 : 0);
    
    player.facts.scoringInputs.structuralRedFlagCount = structuralFlags;
    player.facts.scoringInputs.degenerativeBurdenScore = degenerativeScore;
    player.facts.scoringInputs.instabilityBurdenScore = instabilityScore;
    
    // Recalculate score for this player
    const result = calculateMSI(player.facts);
    player.score = result.msi;
    player.scoreBreakdown = result.breakdown;
    
    console.log('Score updated:', player.score, 'Breakdown:', player.scoreBreakdown);
    
    // Close modal first
    bootstrap.Modal.getInstance(document.getElementById('editFactsModal')).hide();
    
    // Force re-render with a small delay to ensure modal is closed
    setTimeout(() => {
        renderPlayerDashboard(currentPlayerView);
        renderCompareTable();
        showToast(`Facts updated! New score: ${player.score}`);
    }, 100);
});

function showEvidence(docName, page, snippet) {
    document.getElementById('evidenceContent').innerHTML = `
    <p><strong>Document:</strong> ${docName}</p>
    <p><strong>Page:</strong> ${page}</p>
    <p><strong>Excerpt:</strong></p>
    <blockquote class="blockquote bg-light p-3 rounded">${snippet}</blockquote>
    `;
    new bootstrap.Modal(document.getElementById('evidenceModal')).show();
}

// ========== TAB C: COMPARE PLAYERS ==========
function renderCompareCheckboxes() {
    const container = document.getElementById('compareCheckboxes');
    container.innerHTML = '';
    players.forEach(p => {
    const div = document.createElement('div');
    div.className = 'form-check';
    div.innerHTML = `
        <input class="form-check-input compare-checkbox" type="checkbox" value="${p.id}" id="cmp${p.id}">
        <label class="form-check-label" for="cmp${p.id}">${p.name} (${p.pos})</label>
    `;
    container.appendChild(div);
    });

    document.querySelectorAll('.compare-checkbox').forEach(cb => {
    cb.addEventListener('change', (e) => {
        const id = parseInt(e.target.value);
        if (e.target.checked) {
        selectedComparePlayers.add(id);
        } else {
        selectedComparePlayers.delete(id);
        }
        renderCompareTable();
    });
    });
}

function renderCompareTable() {
    const tbody = document.getElementById('compareTableBody');
    tbody.innerHTML = '';

    const selected = players.filter(p => selectedComparePlayers.has(p.id));
    selected.forEach(p => {
        const scoreInfo = getScoreLabel(p.score);
        const counts = p.facts.summaryCounts || {};
        const flags = p.facts.flags || {};
        
        const row = document.createElement('tr');
        row.innerHTML = `
            <td><strong>${p.name}</strong></td>
            <td>${p.pos}</td>
            <td>${p.draftYear}</td>
            <td>${counts.surgeriesTotal || 0}</td>
            <td>${(counts.concussionsTotal || 0) > 0 ? '<span class="badge bg-warning">Yes</span>' : '<span class="badge bg-success">No</span>'}</td>
            <td>${counts.recurrenceTotal || 0}</td>
            <td>
            ${flags.cartilageDegeneration ? '<span class="badge bg-danger me-1">Cartilage</span>' : ''}
            ${flags.looseBodies ? '<span class="badge bg-danger me-1">Loose Bodies</span>' : ''}
            ${flags.osteoarthritisOrArthrosis ? '<span class="badge bg-danger me-1">Arthritis</span>' : ''}
            ${(counts.cervicalNeurologicEventsTotal || 0) > 0 ? '<span class="badge bg-danger me-1">Cervical</span>' : ''}
            ${!flags.cartilageDegeneration && !flags.looseBodies && !flags.osteoarthritisOrArthrosis && !(counts.cervicalNeurologicEventsTotal > 0) ? '<span class="text-muted">None</span>' : ''}
            </td>
            <td>${counts.missedGamesTotal || 0}</td>
            <td><span class="badge bg-${scoreInfo.badge} fs-6">${p.score}</span></td>
        `;
        tbody.appendChild(row);
    });
}

// Sorting
document.querySelectorAll('.sortable').forEach(th => {
    th.style.cursor = 'pointer';
    th.addEventListener('click', () => {
    const sortKey = th.dataset.sort;
    sortCompareTable(sortKey);
    });
});

function sortCompareTable(key) {
    const selected = Array.from(selectedComparePlayers);
    selected.sort((a, b) => {
    const pA = players.find(p => p.id === a);
    const pB = players.find(p => p.id === b);
    if (key === 'score') return pB.score - pA.score;
    if (key === 'surgeries') return pB.facts.surgeries - pA.facts.surgeries;
    if (key === 'recurrence') return pB.facts.recurrenceCount - pA.facts.recurrenceCount;
    if (key === 'missedGames') return pB.facts.missedGames - pA.facts.missedGames;
    return 0;
    });
    selectedComparePlayers = new Set(selected);
    renderCompareTable();
}

// Export
document.getElementById('exportJSON').addEventListener('click', (e) => {
    e.preventDefault();
    const selected = players.filter(p => selectedComparePlayers.has(p.id));
    const data = selected.map(p => {
        const counts = p.facts.summaryCounts || {};
        const flags = p.facts.flags || {};
        return {
            name: p.name,
            draftYear: p.draftYear,
            surgeries: counts.surgeriesTotal || 0,
            concussionHistory: (counts.concussionsTotal || 0) > 0,
            recurringInjuries: counts.recurrenceTotal || 0,
            majorImagingFlags: [
                flags.cartilageDegeneration ? 'Cartilage' : null,
                flags.looseBodies ? 'Loose Bodies' : null,
                flags.osteoarthritisOrArthrosis ? 'Arthritis' : null,
                (counts.cervicalNeurologicEventsTotal || 0) > 0 ? 'Cervical' : null
            ].filter(Boolean),
            missedGames: counts.missedGamesTotal || 0,
            medicalScore: p.score,
            scoreBreakdown: p.scoreBreakdown
        };
    });
    downloadFile('comparison.json', JSON.stringify(data, null, 2));
});

document.getElementById('exportCSV').addEventListener('click', (e) => {
    e.preventDefault();
    const selected = players.filter(p => selectedComparePlayers.has(p.id));
    let csv = 'Name,Position,Draft Year,Surgeries,Concussion History,Recurring Injuries,Major Imaging Flags,Missed Games,Medical Score\n';
    selected.forEach(p => {
        const counts = p.facts.summaryCounts || {};
        const flags = p.facts.flags || {};
        const flagsList = [
            flags.cartilageDegeneration ? 'Cartilage' : null,
            flags.looseBodies ? 'Loose Bodies' : null,
            flags.osteoarthritisOrArthrosis ? 'Arthritis' : null,
            (counts.cervicalNeurologicEventsTotal || 0) > 0 ? 'Cervical' : null
        ].filter(Boolean).join('; ');
        csv += `${p.name},${p.pos},${p.draftYear},${counts.surgeriesTotal || 0},${(counts.concussionsTotal || 0) > 0 ? 'Yes' : 'No'},${counts.recurrenceTotal || 0},"${flagsList}",${counts.missedGamesTotal || 0},${p.score}\n`;
    });
    downloadFile('comparison.csv', csv);
});

function downloadFile(filename, content) {
    const blob = new Blob([content], { type: 'text/plain' });
    const url = URL.createObjectURL(blob);
    const a = document.createElement('a');
    a.href = url;
    a.download = filename;
    a.click();
    URL.revokeObjectURL(url);
}

// ========== TOAST ==========
function showToast(message, type = 'success') {
    const toastContainer = document.createElement('div');
    toastContainer.className = 'position-fixed bottom-0 end-0 p-3';
    toastContainer.style.zIndex = 11;
    const bgClass = type === 'danger' ? 'bg-danger text-white' : '';
    toastContainer.innerHTML = `
    <div class="toast show ${bgClass}" role="alert">
        <div class="toast-header">
        <strong class="me-auto">${type === 'danger' ? 'Error' : 'Success'}</strong>
        <button type="button" class="btn-close" data-bs-dismiss="toast"></button>
        </div>
        <div class="toast-body">${message}</div>
    </div>
    `;
    document.body.appendChild(toastContainer);
    setTimeout(() => toastContainer.remove(), 5000);
}

// ========== INIT ==========
renderPlayerSelector();
renderCompareCheckboxes();






