import React, { useEffect, useState } from 'react';
import {
  ColumnDef,
  // ColumnFiltersState,
  SortingState,
  VisibilityState,
  flexRender,
  getCoreRowModel,
  getFilteredRowModel,
  getPaginationRowModel,
  getSortedRowModel,
  useReactTable,
} from '@tanstack/react-table';
import { ChevronDown } from 'lucide-react';

import { Button } from '@/ui/button';
import {
  DropdownMenu,
  DropdownMenuCheckboxItem,
  DropdownMenuContent,
  DropdownMenuItem,
  DropdownMenuTrigger,
} from '@/ui/dropdown-menu';

import {
  Table,
  TableBody,
  TableCell,
  TableHead,
  TableHeader,
  TableRow,
} from '@/ui/table';

import { trpc } from '~/utils/trpc';
import {
  customEmailTruncateHandler,
  customTruncateHandler,
  displayDate,
} from '~/utils/helper';
import { getCustomerFilterSchema } from '~/schema/customer';
import {
  Tooltip,
  TooltipContent,
  TooltipProvider,
  TooltipTrigger,
} from '~/components/ui/tooltip';
import { useToast } from '~/components/ui/use-toast';
import { ScrollArea, ScrollBar } from '~/components/ui/scroll-area';

import { LoadingDialog } from '../modal/loadingModal';
import { MoreHorizontal } from 'lucide-react';
import {
  Select,
  SelectContent,
  SelectItem,
  SelectTrigger,
  SelectValue,
} from '~/components/ui/select';
import {
  DoubleArrowLeftIcon,
  DoubleArrowRightIcon,
} from '@radix-ui/react-icons';
import { ChevronLeftIcon, ChevronRightIcon } from '@radix-ui/react-icons';
import { useRouter } from 'next/router';
import { InvoiceDialog } from '../modal/invoiceModal';
// import { getHtmlContent } from '~/server/clientControllers/pdf';
import { getHtmlContent } from '~/utils/helper';
import { ResourceUploadDialog } from '../modal/resourceModal';
import { CustomerUploadDialog } from '../modal/customerModal';
import Link from 'next/link';

export type Resources = {
  id: number;
  name: string;
  material_type: string;
  truck_cap: string;
  total_rounds: number;
  price: number;

  is_deleted: boolean;
  created_at: Date;
  start_date: Date;
  delivery_date: Date;
};

const initialFilters: any = {
  first: 0,
  rows: 10,
};
type customerDataTableType = {
  type: string;
};

export default function ProjectsDataTable(props: customerDataTableType) {
  console.log('check', { props });
  // use toast
  const { toast } = useToast();
  const router = useRouter();
  const [sorting, setSorting] = useState<SortingState>([]);
  const [filterID, setFilterID] = useState({});

  const [filters, setFilters] =
    useState<getCustomerFilterSchema>(initialFilters);
  const [columnVisibility, setColumnVisibility] = useState<VisibilityState>({});
  const [openInvoice, setIsInvoice] = React.useState(false);
  const [rowSelection, setRowSelection] = useState({});
  const [selectedItem, setSelectedItem] = React.useState({});
  const [invoiceId, setInvoiceId] = React.useState<number>(0);
  const [invoiceLoader, setInvoiceLoader] = React.useState(false);
  const [isModal, setIsModal] = React.useState(false);

  async function getInvoiceContent(id: number) {
    setInvoiceId(id);
    setIsInvoice(true);
  }

  // APi
  const { data, refetch, isLoading } = trpc.project.get.useQuery(
    {
      ...filters,
      type: props?.type,
      filters: { ...filterID },
    },
    {
      refetchOnWindowFocus: false,
    },
  );
  console.log({ data });

  const rolesData = React.useMemo(() => {
    return Array.isArray(data?.data) ? data?.data : [];
  }, [data]);

  function openChangeHandler(data: any) {
    setIsModal((prevState) => !prevState);
    if (data) {
      setSelectedItem({ id: data?.id, name: data?.name, code: data?.code });
    } else {
      setSelectedItem({});
    }
  }
  // handle modal
  // const handleEnbled = (data: any) => {
  //   setSelectedItem({ id: data?.id, name: data?.name, code: data?.code });
  //   openChangeHandler();
  // };
  // columns
  const columns: ColumnDef<Resources>[] = [
    {
      accessorKey: 'name',
      header: 'Name',
      cell: ({ row }) => {
        return (
          <div className="flex items-center gap-4 text-ellipsis whitespace-nowrap overflow-hidden">
            <TooltipProvider>
              <Tooltip>
                <TooltipTrigger>
                  <p className="capitalize">{row?.original?.name}</p>
                </TooltipTrigger>
                <TooltipContent>
                  <p className="text-base font-normal">{row?.original?.name}</p>
                </TooltipContent>
              </Tooltip>
            </TooltipProvider>
          </div>
        );
      },
    },
    {
      accessorKey: 'material',
      header: 'Material Type',
      cell: ({ row }) => {
        return (
          <div className="flex items-center gap-4 text-ellipsis whitespace-nowrap overflow-hidden capitalize">
            <TooltipProvider>
              <Tooltip>
                <TooltipTrigger>
                  <p className="capitalize">{row?.original?.material_type}</p>
                  {/* {customEmailTruncateHandler(row?.original?.email)} */}
                </TooltipTrigger>
                <TooltipContent>
                  <p className="text-base font-normal">
                    {row?.original?.material_type}
                  </p>
                </TooltipContent>
              </Tooltip>
            </TooltipProvider>
          </div>
        );
      },
    },
    {
      accessorKey: 'capacity',
      header: 'Truck Capacity',
      cell: ({ row }) => {
        return (
          <div className="flex items-center gap-4 capitalize text-ellipsis whitespace-nowrap overflow-hidden">
            {row?.original?.truck_cap}
          </div>
        );
      },
    },
    {
      accessorKey: 'rounds',
      header: 'Total Rounds',
      cell: ({ row }) => {
        return (
          <div className="flex items-center gap-4 text-ellipsis whitespace-nowrap overflow-hidden">
            {row?.original?.total_rounds}
          </div>
        );
      },
    },
    {
      accessorKey: 'price',
      header: 'Project Price',
      cell: ({ row }) => {
        return (
          <div className="flex items-center gap-4 text-ellipsis whitespace-nowrap overflow-hidden">
            $ {row?.original?.price}
          </div>
        );
      },
    },
    {
      accessorKey: 'Start Date',
      header: 'Start Date',
      cell: ({ row }) => (
        <div className="capitalize text-ellipsis whitespace-nowrap ">
          {displayDate(row?.original?.start_date)}
        </div>
      ),
    },
    {
      accessorKey: 'Delivery Date',
      header: 'Delivery Date',
      cell: ({ row }) => (
        <div className="capitalize text-ellipsis whitespace-nowrap ">
          {displayDate(row?.original?.delivery_date)}
        </div>
      ),
    },
    {
      accessorKey: 'Created Date',
      header: 'Created Date',
      cell: ({ row }) => (
        <div className="capitalize text-ellipsis whitespace-nowrap ">
          {displayDate(row?.original?.created_at)}
        </div>
      ),
    },

    {
      id: 'actions',
      enableHiding: false,
      header: 'Actions',
      cell: ({ row }) => {
        return (
          <DropdownMenu>
            <DropdownMenuTrigger asChild>
              <Button variant="ghost" className="h-8 w-8 p-0">
                <span className="sr-only">Open menu</span>
                <MoreHorizontal className="h-4 w-4" />
              </Button>
            </DropdownMenuTrigger>
            <DropdownMenuContent align="end">
              {/* <DropdownMenuLabel>Actions</DropdownMenuLabel> */}
              {/* <DropdownMenuSeparator /> */}
              <DropdownMenuItem className=" cursor-pointer">
                <Link href={`/admin/projects/detail/${row?.original?.id}`}>
                  View Project
                </Link>
              </DropdownMenuItem>
              {props.type === 'closed' && (
                <DropdownMenuItem className=" cursor-pointer">
                  <button onClick={() => getInvoiceContent(row?.original?.id)}>
                    View Invoice
                  </button>
                </DropdownMenuItem>
              )}
            </DropdownMenuContent>
          </DropdownMenu>
        );
      },
    },
  ];

  const table = useReactTable({
    data: rolesData as Resources[],
    columns,
    onSortingChange: setSorting,
    getCoreRowModel: getCoreRowModel(),
    getPaginationRowModel: getPaginationRowModel(),
    getSortedRowModel: getSortedRowModel(),
    getFilteredRowModel: getFilteredRowModel(),
    onColumnVisibilityChange: setColumnVisibility,
    onRowSelectionChange: setRowSelection,
    state: {
      sorting,
      columnVisibility,
      rowSelection,
    },
  });

  function handlePagination(page: number) {
    if (page < 0) return;
    setFilters((prevFilters) => ({ ...prevFilters, first: page }));
  }

  // FILTER OPTIONS
  const roleOptions1 = [
    {
      Icon: 'fal fa-chevron-down',
      text: 'Search',
      filtername: 'searchQuery',
      type: 'text',
    },

    {
      Icon: 'fal fa-chevron-down',
      text: 'Enabled',
      filtername: 'is_enabled',
      type: 'select',

      filter: [
        {
          name: 'Yes',
          value: true,
        },
        {
          name: 'No',
          value: false,
        },
      ],
    },
    {
      Icon: 'fal fa-chevron-down',
      text: 'Type',
      filtername: 'is_percentage',
      type: 'select',

      filter: [
        {
          name: 'Fixed',
          value: false,
        },
        {
          name: 'Percentage',
          value: true,
        },
      ],
    },
    {
      Icon: 'fal fa-chevron-down',
      text: 'Limit',
      filtername: 'is_limited',
      type: 'select',

      filter: [
        {
          name: 'Limit',
          value: true,
        },
        {
          name: 'Unlimited',
          value: false,
        },
      ],
    },
    {
      Icon: 'fal fa-chevron-down',
      text: 'Start Date',
      filtername: 'startDate',
      type: 'date',
    },
    {
      Icon: 'fal fa-chevron-down',
      text: 'End Date',
      filtername: 'endDate',
      type: 'date',
    },
    {
      Icon: 'fal fa-chevron-down',
      text: 'Clear Filter',
      filtername: 'Clear',
    },
  ];
  useEffect(() => {
    refetch();
  }, [props?.type]);

  return (
    <div className="w-full space-y-4">
      <div className="flex items-center justify-between mb-4">
        <div className=" text-4xl font-semibold capitalize">
          {/* {props?.type ?? 'Customers'} */}
        </div>
      </div>
      <div className="flex items-center justify-end gap-2">
        <DropdownMenu>
          <DropdownMenuTrigger asChild>
            <Button variant="outline">
              Columns <ChevronDown className="ml-2 h-4 w-4" />
            </Button>
          </DropdownMenuTrigger>
          <DropdownMenuContent align="end">
            {table
              .getAllColumns()
              .filter((column) => column.getCanHide())
              .map((column) => {
                return (
                  <DropdownMenuCheckboxItem
                    key={column.id}
                    className="capitalize"
                    checked={column.getIsVisible()}
                    onCheckedChange={(value) =>
                      column.toggleVisibility(!!value)
                    }
                  >
                    {column.id}
                  </DropdownMenuCheckboxItem>
                );
              })}
          </DropdownMenuContent>
        </DropdownMenu>
      </div>
      <div className="rounded-md border border-border ">
        <ScrollArea className="w-full ">
          <ScrollBar orientation="horizontal"></ScrollBar>

          <Table className="w-full overflow-x-scroll">
            <TableHeader className="bg-secondary/80">
              {table?.getHeaderGroups().map((headerGroup) => (
                <TableRow key={headerGroup.id}>
                  {headerGroup.headers.map((header) => {
                    return (
                      <TableHead key={header.id}>
                        {header.isPlaceholder
                          ? null
                          : flexRender(
                              header.column.columnDef.header,
                              header.getContext(),
                            )}
                      </TableHead>
                    );
                  })}
                </TableRow>
              ))}
            </TableHeader>
            <TableBody>
              {table?.getRowModel()?.rows?.length ? (
                table?.getRowModel()?.rows?.map((row) => (
                  <TableRow
                    key={row.id}
                    data-state={row.getIsSelected() && 'selected'}
                  >
                    {row.getVisibleCells().map((cell) => (
                      <TableCell key={cell.id}>
                        {flexRender(
                          cell.column.columnDef.cell,
                          cell.getContext(),
                        )}
                      </TableCell>
                    ))}
                  </TableRow>
                ))
              ) : (
                <TableRow>
                  <TableCell
                    colSpan={columns.length}
                    className="h-24 text-center"
                  >
                    No results.
                  </TableCell>
                </TableRow>
              )}
            </TableBody>
          </Table>
        </ScrollArea>
      </div>
      <div className="flex items-center justify-end space-x-2 py-4">
        <div className="flex-1 flex w-[100px] items-center justify-start text-sm font-medium">
          Page {filters.first + 1} of{' '}
          {Math.ceil((data?.count ?? 0) / filters.rows)}
        </div>

        <div className="flex items-center justify-center space-x-6 lg:space-x-8">
          <div className="flex items-center space-x-2">
            <p className="text-sm font-medium">Rows per page</p>
            <Select
              value={`${filters.rows}`}
              onValueChange={(value) => {
                setFilters((prevFilters: any) => ({
                  ...prevFilters,
                  rows: Number(value),
                  first: 0,
                }));
                table.setPageSize(Number(value));
              }}
            >
              <SelectTrigger className="h-8 w-[70px]">
                <SelectValue placeholder={filters.rows} />
              </SelectTrigger>
              <SelectContent side="top">
                {[5, 10, 20, 30, 40, 50].map((pageSize) => (
                  <SelectItem key={pageSize} value={`${pageSize}`}>
                    {pageSize}
                  </SelectItem>
                ))}
              </SelectContent>
            </Select>
          </div>

          <div className="flex items-center space-x-2">
            <Button
              variant="outline"
              className="hidden h-8 w-8 p-0 lg:flex"
              onClick={() => handlePagination(0)}
              disabled={filters.first === 0}
            >
              <span className="sr-only">Go to first page</span>
              <DoubleArrowLeftIcon className="h-4 w-4" />
            </Button>

            <Button
              variant="outline"
              className="h-8 w-8 p-0"
              onClick={() => handlePagination(filters?.first - 1)}
              disabled={filters?.first === 0}
            >
              <span className="sr-only">Go to previous page</span>
              <ChevronLeftIcon className="h-4 w-4" />
            </Button>
            <Button
              variant="outline"
              className="h-8 w-8 p-0"
              onClick={() => handlePagination(filters.first + 1)}
              disabled={
                (filters.first + 1) * filters.rows > (data?.count ?? 0) ||
                Math.ceil((data?.count ?? 0) / filters.rows) ==
                  filters.first + 1
              }
            >
              <span className="sr-only">Go to next page</span>
              <ChevronRightIcon className="h-4 w-4" />
            </Button>

            <Button
              variant="outline"
              className="hidden h-8 w-8 p-0 lg:flex"
              onClick={() =>
                handlePagination(
                  Math.ceil((data?.count ?? 0) / filters.rows) - 1,
                )
              }
              disabled={
                (filters.first + 1) * filters.rows > (data?.count ?? 0) ||
                Math.ceil((data?.count ?? 0) / filters.rows) ==
                  filters.first + 1
              }
            >
              <span className="sr-only">Go to last page</span>
              <DoubleArrowRightIcon className="h-4 w-4" />
            </Button>
          </div>
        </div>
      </div>
      <InvoiceDialog
        open={openInvoice}
        setIsModal={setIsInvoice}
        invoiceId={invoiceId}
        loader={invoiceLoader}
        setLoader={setInvoiceLoader}
      />
      <LoadingDialog open={isLoading} text={'Loading data...'} />
    </div>
  );
}
