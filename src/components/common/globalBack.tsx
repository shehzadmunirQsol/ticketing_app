import React from 'react'
import { useRouter } from 'next/router'
import { Button } from '../ui/button'

const GlobalBack = () => {
    const router = useRouter()
    return (
        <Button  onClick={() => router.back()} className='rounded-md  bg-muted'>
            <i className="fas text-lg fa-arrow-left-long text-white "></i>
        </Button>
    )
}
export default GlobalBack