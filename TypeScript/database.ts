import { v5 } from "https://deno.land/std@0.106.0/uuid/mod.ts";

type Type<D> = {
    type: string
    data: D
}

type Entry<T extends Type<D>,D> = {
    uuid: string
    type: T["type"]
    data: D
}

type DB<T extends Type<D>,D> = Record<string,Omit<Entry<T,D>,"uuid">>

async function insert<T extends Type<D>,D>(db: DB<T,D>, type: T["type"], data: D): Promise<DB<T,D>> {
    return {
        ...db,
        [await v5.generate(type, (new TextEncoder()).encode(crypto.randomUUID()))]: {
            type: type,
            data: data
        }
    }
}