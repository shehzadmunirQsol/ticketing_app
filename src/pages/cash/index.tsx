import React from 'react'
import { NextPageWithLayout } from '../_app'
import CashPage from '~/components/app/cash'

const IndexPage: NextPageWithLayout = () => {
  return (
    <div className="bg-background h-auto justify-center items-center  w-screen   ">
      <div className="">
        <CashPage />
      </div>
    </div>
  )
}

export default IndexPage