# interview-merge

Merges subtitle files in SRT format (each representing a different speaker) into
a single file for [oTranscribe](http://otranscribe.com/).

## Usage

List speaker names and file paths like so, piping the output into an OTR file:

```
stack exec interview-merge -- Me captions-me.srt You captions-you.srt > transcript.otr
```
