import { createSourceFile, ScriptTarget } from 'typescript'
export function parse(s: string) {
    return createSourceFile('fake.ts', s, ScriptTarget.ES5)
}
