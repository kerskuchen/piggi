// deno-lint-ignore-file prefer-const

// Zero based line and column indices
export class LineColumnIndex
{
    public constructor(
        public line: number,
        public column: number,
    ) { }
}

export class Source
{
    public constructor(
        public modulename: string,
        public filepath: string,
        public content: string
    ) { }

    GetLineColumnIndex(charPos: number): LineColumnIndex
    {
        let lineIndex = 0
        let columnIndex = 0
        let curPos = 0
        while (curPos < charPos) {
            if (this.content[curPos] == '\n') {
                lineIndex += 1
                columnIndex = 0
            } else {
                columnIndex += 1
            }
            curPos += 1
        }
        return new LineColumnIndex(lineIndex, columnIndex)
    }

    GetLineLocation(lineIndex: number): SourceLocation
    {
        let lineCounter = 0
        let startPos = 0
        while (startPos < this.content.length) {
            if (lineCounter == lineIndex)
                break
            if (this.content[startPos] == '\n') {
                lineCounter += 1
            }
            startPos += 1
        }

        let endPos = startPos
        while (endPos < this.content.length) {
            if (this.content[endPos] == '\n') {
                endPos += 1
                break
            }
            endPos += 1
        }
        return SourceLocation.FromStartEnd(this, startPos, endPos)
    }

    GetLineTextWithoutLineBreak(lineIndex: number): string
    {
        let result = this.GetLineLocation(lineIndex).GetText()
        if (result.endsWith("\r\n"))
            return result.substring(0, result.length - 2)
        else if (result.endsWith("\n"))
            return result.substring(0, result.length - 1)
        else
            return result
    }
}

export class SourceLocation
{
    private constructor(
        public source: Source,
        public start: number,
        public length: number
    ) { }

    static FromStartLength(source: Source, start: number, length: number)
    {
        return new SourceLocation(source, start, length)
    }
    static FromStartEnd(source: Source, start: number, end: number)
    {
        return new SourceLocation(source, start, end - start)
    }

    get end(): number { return this.start + this.length }
    GetText(): string { return this.source.content.substring(this.start, this.end) }

    static Compare(a: SourceLocation, b: SourceLocation): number
    {
        if (a.source.filepath !== b.source.filepath)
            return a.source.filepath < b.source.filepath ? -1 : 1

        if (a.start === b.start)
            return a.length - b.length
        else
            return a.start - b.start
    }

    OverlapsWith(other: SourceLocation): boolean
    {
        if (this.source.filepath !== other.source.filepath)
            return false

        return this.start < other.end && this.end > other.start
    }
}

export class Diagnostic
{
    constructor(
        public location: SourceLocation,
        public message: string
    ) { }

    static Compare(a: Diagnostic, b: Diagnostic): number
    {
        return SourceLocation.Compare(a.location, b.location)
    }

    Print()
    {
        let location = this.location
        let source = location.source
        let filepath = location.source.filepath

        let startLineColumnIndex = source.GetLineColumnIndex(location.start)
        let endLineColumnIndex = source.GetLineColumnIndex(location.end)

        // TODO: This does not work for multiline locations yet
        let line = source.GetLineLocation(startLineColumnIndex.line).GetText()
        if (!line.endsWith('\n'))
            line += '\n'
        let prefix = line.substring(0, startLineColumnIndex.column)
        let error = line.substring(startLineColumnIndex.column, endLineColumnIndex.column)
        let suffix = line.substring(endLineColumnIndex.column)

        let message = ""
        message += `${filepath}:${startLineColumnIndex.line + 1}:${startLineColumnIndex.column + 1}: "${this.message}"\n`
        message += `${prefix}${error}${suffix}`
        message += `${prefix.replaceAll(/./g, " ")}${error.replaceAll(/./g, "~").replace("~", "^")}${suffix.replaceAll(/./g, " ")}`
        console.error(message)
    }
}

export class DiagnosticBag
{
    diagnostics: Diagnostic[] = []

    get hasErrors() { return this.diagnostics.length != 0 }
    get count() { return this.diagnostics.length }

    Append(other: DiagnosticBag)
    {
        this.diagnostics = this.diagnostics.concat(other.diagnostics)
    }

    Prepend(other: DiagnosticBag)
    {
        this.diagnostics = other.diagnostics.concat(this.diagnostics)
    }

    ReportError(location: SourceLocation, message: string)
    {
        let diagnostic = new Diagnostic(location, message)

        // TODO: remove this when not testing
        diagnostic.Print()
        Deno.exit()

        this.diagnostics.push(diagnostic)
    }

    Print(maxErrorsToPrint = 1000)
    {
        let numPrinted = 0
        for (let diagnostic of this.diagnostics.toSorted(Diagnostic.Compare)) {
            if (numPrinted > maxErrorsToPrint)
                break

            diagnostic.Print()
            numPrinted += 1
        }
    }
}

////////////////////////////////////////////////////////////////////////////////////////////////////
// IndentedTextWriter

export class IndentedTextWriter
{
    buffer = ""
    private indentLevel = 0

    constructor(private indentLength = 4) { }

    Write(text: string)
    {
        this.buffer += text
    }

    NewLine()
    {
        this.buffer += "\n" + this.GetIndentString(this.indentLevel)
    }

    Indent()
    {
        this.indentLevel += 1
    }

    Unindent()
    {
        if (this.indentLevel == 0)
            throw new Error("Invalid indentation")
        this.indentLevel -= 1
    }

    GetIndentString(indent: number)
    {
        let result = ""
        for (let index = 0; index < indent; index++) {
            for (let width = 0; width < this.indentLength; width++) {
                result += " "
            }
        }
        return result
    }
}
